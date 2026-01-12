open Client.Client
open Xapi_client_util

module D = Debug.Make (struct let name = "quicktest_host_vm_properties" end)

(* Host properties related to the VM's running on it: memory usage, evacuation *)

let host_ref rpc = Ref.t_of_rpc (fun _ -> `host) rpc

let if_asserted_host t ~host (assertf : unit api)
    (f : (host:API.ref_host -> API.ref_task) api) =
  match call t @@ assertf with
  | () ->
      Some (task t ignore @@ f ~host)
  | exception Api_errors.Server_error _ ->
      None

let if_asserted_vm t ~vm (assertf : unit api)
    (f : (vm:API.ref_VM -> API.ref_task) api) =
  match call t @@ assertf with
  | () ->
      Some (task t ignore @@ f ~vm)
  | exception Api_errors.Server_error _ ->
      None

let get_management_network t ~host =
  let pif = call t @@ Host.get_management_interface ~host in
  call t @@ PIF.get_network ~self:pif

let maybe_evacuate t ~host =
  let network = get_management_network t ~host in
  if_asserted_host t ~host Host.(assert_can_evacuate ~host)
  @@ Async.Host.evacuate ~network ~evacuate_batch_size:0L

let with_host_disabled t ~host f =
  call t @@ Host.disable ~host ~auto_enable:false ;
  let finally () = call t @@ Host.enable ~host in
  Fun.protect ~finally @@ fun () -> f t ~host

let xen_localhost_memory_free_pages () =
  Xenctrl.with_intf @@ fun xc ->
  let host_info = Xenctrl.physinfo xc in
  host_info.Xenctrl.free_pages |> Int64.of_nativeint

let xen_localhost_memory_free_bytes () =
  let pages = xen_localhost_memory_free_pages () in
  let kib = Xenctrl.pages_to_kib pages in
  Int64.shift_left kib 10

let rec wait_no_tasks t ~host =
  let tasks =
    call t @@ Task.get_all_records
    |> List.filter_map @@ fun (taskref, task) ->
       if
         task.API.task_resident_on = host
         && (task.API.task_status = `pending
            || task.API.task_status = `cancelling
            )
       then
         Some taskref
       else
         None
  in
  match tasks with
  | [] ->
      ()
  | _ ->
      (* we can't efficiently wait for cancelling -> cancelled *)
      Thread.delay 0.1 ;
      (* TODO: use run_or_cancel equiv with cli progress *)
      D.debug "Waiting for %d tasks to complete on localhost" (List.length tasks) ;
      call t @@ Tasks.wait_for_all ~tasks ;
      wait_no_tasks t ~host

let bytes_per_gib = Int64.shift_left 1L 30 |> Int64.to_float

let print_mem () bytes =
  let kib = Int64.shift_right bytes 10
  and gib = Int64.to_float bytes /. bytes_per_gib in
  Printf.sprintf "%Ld bytes = %Ld KiB = %.6f GiB" bytes kib gib

(* TODO: generalize this to computation, proof, preconditions, etc. *)

let test_free_memory t dummy_vm =
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  let failed =
    with_host_disabled t ~host @@ fun t ~host ->
    wait_no_tasks t ~host ;
    let computed_free = call t @@ Host.compute_free_memory ~host
    and actual_free = xen_localhost_memory_free_bytes () in
    D.debug "Host computed_free: %a, actual free: %a" print_mem computed_free
      print_mem actual_free ;
    if computed_free < actual_free then (
      D.warn
        "Host has less free memory than computed: %Ld < %Ld. This could cause \
         VM starts, or incoming VM migrations to fail"
        computed_free actual_free ;
      Some computed_free
    ) else
      None
  in
  Option.bind failed @@ fun computed_free ->
  (* Try to prove it is an actual problem: start a VM that uses all the claimed
     available memory *)
  let self = call t @@ VM.clone ~vm:dummy_vm ~new_name:"free memory test" in
  let value =
    call t @@ VM.maximise_memory ~self ~approximate:false ~total:computed_free
  in
  call t @@ VM.set_memory ~self ~value ;
  if_asserted_vm t ~vm:self VM.(assert_can_boot_here ~host ~self)
  @@ Async.VM.start_on ~host ~start_paused:true ~force:false

(* also might temporarily need more mem, so have a memory watcher... *)
