open Client.Client
open Xapi_client_util

module D = Debug.Make (struct let name = "quicktest_host_vm_properties" end)

let memory_min = Int64.shift_left 1L 28

let points_between var_min var_max =
  Seq.append
    (Seq.unfold
       (fun i ->
         if i < var_max then
           Some (i, Int64.shift_left i 1)
         else
           None
       )
       var_min
    )
    (Seq.return var_max)

let free_pages = Quicktest_host_vm_properties.xen_localhost_memory_free_pages

let monitor_free_memory =
  Thread.create @@ fun (minimum, stop) ->
  minimum := free_pages () ;
  while not @@ Atomic.get stop do
    minimum := min !minimum @@ free_pages () ;
    Thread.delay 0.001
  done ;
  !minimum

let with_monitor_free_memory f =
  let stop = Atomic.make false in
  let minimum = ref Int64.max_int in
  let thread = monitor_free_memory (minimum, stop) in
  let finally () = Atomic.set stop true in
  Fun.protect ~finally f ;
  Thread.join thread ;
  let final = free_pages () in
  let extra = max 0L (Int64.sub final !minimum) in
  (* TODO: check whether all this extra overhead was covered in
     memory_overhead/etc. *)
  D.debug
    "Final memory usage: %Ld pages, minimum: %Ld pages, extra overhead: %Ld \
     pages"
    final !minimum extra

let calibrate rpc session_id vm_template var_name var_set var_min var_max () =
  let t = {rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template:vm_template @@ fun vm ->
  (* start with a small VM *)
  call t @@ VM.set_memory ~self:vm ~value:memory_min ;
  let var_min = var_min t and var_max = var_max t ~vm in
  let measure value =
    var_set t ~self:vm ~value ;
    let () =
      with_monitor_free_memory @@ fun () ->
      call t @@ VM.start_on ~host ~start_paused:true ~force:false ~vm
    in
    let free =
      Quicktest_host_vm_properties.xen_localhost_memory_free_pages ()
    in
    D.debug "VM %s: %Ld => %Ld free memory pages" var_name value free ;
    call t @@ VM.hard_shutdown ~vm ;
    free
  in
  (* a previous test may not have cleaned up properly, wait for any pending
     tasks to finish, so we get an "idle" host *)
  Quicktest_host_vm_properties.wait_no_tasks t ~host ;
  D.debug "Measuring VM %s impact on memory usage between %Ld and %Ld" var_name
    var_min var_max ;
  let baseline = measure var_min in
  let max_coeff =
    points_between var_min var_max
    |> Seq.map (fun x -> (x, measure x))
    |> Seq.map (fun (x, y) -> (Int64.sub x var_min, Int64.sub baseline y))
    |> Seq.map (fun (x, y) ->
           if x > 0L then Int64.to_float y /. Int64.to_float x else 0.
       )
    |> Seq.fold_left Float.max 0.
  in
  (* we want this to always be an overestimate, so round accordingly *)
  let op, max_coeff_int =
    if Float.round max_coeff >= 1. then
      ("*", Float.ceil max_coeff |> Int64.of_float)
    else
      ("/", Float.floor (1. /. max_coeff) |> Int64.of_float)
  in
  D.info "VM memory_overhead_pages = ... + %s * %g =~ ... + %s %s %Ld" var_name
    max_coeff var_name op max_coeff_int ;
  ()

let set_vcpu t ~self ~value =
  call t @@ VM.set_VCPUs_max ~self ~value ;
  call t @@ VM.set_VCPUs_at_startup ~self ~value

let current_max_vcpus t ~vm:_ =
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  let host_cpus = call t @@ Host.get_host_CPUs ~self:host |> List.length in
  (* can't have more vCPUs than the host does *)
  min 64 host_cpus |> Int64.of_int

let set_memory t ~self ~value = call t @@ VM.set_memory ~self ~value

let current_max_vm_memory t ~vm =
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  let total = call t @@ Host.compute_free_memory ~host in
  call t @@ VM.maximise_memory ~total ~approximate:false ~self:vm

let variables =
  [
    ("vcpu", set_vcpu, (fun _ -> 1L), current_max_vcpus)
  ; ("memory", set_memory, (fun _ -> memory_min), current_max_vm_memory)
    (* TODO: migration and memory construction, run parallel thread that looks
       for min memory, to measure any temporary overhead, dirty bitmap/etc. *)
  ]

let specialise (name, speed, test) (var_name, var_set, var_min, var_max) =
  let name = Printf.sprintf "%s: %s" name var_name in
  (name, speed, test var_name var_set var_min var_max)

let tests () =
  let open Qt_filter in
  [("VM memory overhead calibration", `Slow, calibrate)]
  |> conn
  |> vm_template Qt.VM.Template.other
  |> List.concat_map (fun tc -> List.map (specialise tc) variables)
