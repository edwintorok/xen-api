open Client.Client

let div_round_up a b = Int64.(div (add a @@ pred b) b)

module RPC = struct
  let src = Logs.Src.create "RPC"

  module Log = (val Logs.src_log src : Logs.LOG)

  (** [wrap rpc] wraps the [rpc] function and logs
      all calls and responses.
   *)
  let wrap rpc call =
    Log.debug (fun m -> m "-> %s" Jsonrpc.(string_of_call call)) ;
    let reply = rpc call in
    Log.debug (fun m -> m "<- %s" Jsonrpc.(string_of_response reply)) ;
    reply
end

let src = Logs.Src.create __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)

module type Variable = sig
  (** type of the variable *)
  type t

  val name : string
  (** name of the variable *)

  val set : client -> vm:API.ref_VM -> t -> unit
  (** [set client ~vm value] API call that sets the variable on [vm] to [value] *)

  val values : client -> host:API.ref_host -> vm:API.ref_VM -> t Seq.t
  (** [values client ~vm] is a sequence of valid values that
      can be used to calibrate the memory overhead for this variable.
   *)

  val to_int64 : t -> int64
  (** [to_int64 t] is the value of [t] as an integer that is used in memory
      overhead calculation.
      This is not necessarily the same as the raw value used by [set].
      *)

  val pp : t Fmt.t
  (** [pp ppf t] pretty prints the value of the variable [t] *)
end

(** [points_between var_min var_max] generates a sequence of values between
    [[var_min, var_max]], where [var_max] is not necessarily a power of 2.
    The points inbetween are [var_min * 2**i]
    *)
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

let free_pages () =
  Xenctrl.with_intf @@ fun xc ->
  let host_info = Xenctrl.physinfo xc in
  host_info.Xenctrl.free_pages |> Int64.of_nativeint

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
      Log.debug (fun m ->
          m "Waiting for %d tasks to complete on localhost" (List.length tasks)
      ) ;
      call t @@ Tasks.wait_for_all ~tasks ;
      wait_no_tasks t ~host

let rec stable_free_pages t ~host =
  let rec loop delay =
    wait_no_tasks t ~host ;
    let v0 = free_pages () in
    (* there may be some pending tasks in Xen, wait for them to finish *)
    Thread.delay delay ;
    let v1 = free_pages () in
    if v1 <> v0 then begin
      Log.debug (fun m ->
          m "Free pages is not stable: %Ld != %Ld (delay: %f)" v0 v1 delay
      ) ;
      loop (delay *. 2.)
    end else
      v1
  in
  loop 0.005

let with_measure_memory_pages t ~host f =
  let pages0 = stable_free_pages t ~host in
  f () ;
  let pages1 = stable_free_pages t ~host in
  let delta = Int64.sub pages0 pages1 in
  Log.debug (fun m ->
      m "Memory usage increase: %Ld - %Ld = %Ld pages" pages1 pages0 delta
  ) ;
  delta

let mib n = Int64.shift_left n 20

let memory_min = mib 256L

let pagesize = Int64.shift_left (Xenctrl.pages_to_kib 1L) 10

let bytes_to_pages bytes = Int64.div bytes pagesize

let vm_ref _ = `VM

let check_tasks t tasks =
  tasks
  |> List.iter @@ fun self ->
     if call t @@ Task.get_status ~self <> `success then
       let err = call t @@ Task.get_error_info ~self in
       Alcotest.failf "Task failed : %s" (String.concat "," err)

(* TODO: use run_or_cancel which raises *)
let clone_vms t ~vm n =
  let tasks =
    List.init n @@ fun i ->
    let new_name = Printf.sprintf "failuretest-%d" i in
    call t @@ Async.VM.clone ~vm ~new_name
  in
  call t @@ Tasks.wait_for_all ~tasks ;
  check_tasks t tasks ;
  tasks
  |> List.map @@ fun self ->
     call t @@ Task.get_result ~self |> Xmlrpc.of_string |> Ref.t_of_rpc vm_ref

let start_vms t ~host vms =
  let tasks =
    vms
    |> List.map @@ fun vm ->
       call t @@ Async.VM.start_on ~host ~vm ~start_paused:true ~force:false
  in
  call t @@ Tasks.wait_for_all ~tasks ;
  check_tasks t tasks

let shutdown_vms t vms =
  let tasks =
    vms |> List.map @@ fun vm -> call t @@ Async.VM.hard_shutdown ~vm
  in
  call t @@ Tasks.wait_for_all ~tasks ;
  check_tasks t tasks

let fill_mem_pow2 t ~host ~vm =
  let free_mem = call t @@ Host.compute_free_memory ~host in
  let sizes =
    Seq.unfold
      (fun total ->
        let half = Int64.div total 2L in
        let next =
          if half < memory_min then
            total
          else
            half
        in
        if next <= 0L then
          None
        else begin
          let value =
            call t @@ VM.maximise_memory ~self:vm ~approximate:false ~total:next
          in
          let overhead = call t @@ VM.compute_memory_overhead ~vm in
          Log.debug (fun m ->
              m "Trying to fill %Ld bytes, VM mem: %Ld, computed overhead = %Ld"
                next value overhead
          ) ;
          call t @@ VM.set_memory ~self:vm ~value ;
          Some (value, Int64.sub total (Int64.add value overhead))
        end
      )
      free_mem
    |> List.of_seq
  in
  let vms = clone_vms t ~vm (List.length sizes) in
  let () =
    List.combine vms sizes
    |> List.iter @@ fun (self, value) -> call t @@ VM.set_memory ~self ~value
  in
  Log.info (fun m -> m "Starting %d VMs" (List.length vms)) ;
  start_vms t ~host vms ;
  shutdown_vms t vms

let try_to_trigger_failure (type a) t ~host ~vm
    (module V : Variable with type t = a) (x : a) vms =
  V.set t ~vm x ;
  let overhead = call t @@ VM.compute_memory_overhead ~vm
  and vm_mem = call t @@ VM.get_memory_dynamic_min ~self:vm in
  let vm_total_mem = Int64.add overhead vm_mem in
  let free_mem = call t @@ Host.compute_free_memory ~host in
  let max_vms = Int64.div free_mem vm_total_mem |> Int64.to_int in
  (* not too many .. *)
  let max_vms = min (min vms max_vms) 500 in
  let vms = clone_vms t ~vm max_vms in
  start_vms t ~host vms ; fill_mem_pow2 t ~host ~vm ; shutdown_vms t vms

let boot1 rpc session_id template (module V : Variable) () =
  let t = {rpc= RPC.wrap rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm ->
  let value = V.values t ~host ~vm |> List.of_seq |> List.rev |> List.hd in
  (* set to largest: most chance to find a bug *)
  V.set t ~vm value ;
  let total = call t @@ Host.compute_free_memory ~host in
  let value = call t @@ VM.maximise_memory ~self:vm ~approximate:false ~total in
  Log.info (fun m -> m "Booting a VM with %Ld bytes memory to fill %Ld bytes" value total);
  call t @@ VM.set_memory ~self:vm ~value ;
  call t @@ VM.assert_can_boot_here ~self:vm ~host ;
  call t @@ VM.start_on ~vm ~host ~force:false ~start_paused:true ;
  call t @@ VM.hard_shutdown ~vm

let calibrate rpc session_id template (module V : Variable) () =
  let t = {rpc= RPC.wrap rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm ->
  (* start with a small VM, [module V] can override it *)
  call t @@ VM.set_memory ~self:vm ~value:memory_min ;

  let actual_free_pages = stable_free_pages t ~host in
  let computed_free_pages =
    call t @@ Host.compute_free_memory ~host |> bytes_to_pages
  in
  Log.debug (fun m ->
      m "Actual free pages (Xen): %Ld, computed free pages\n  (XAPI): %Ld"
        actual_free_pages computed_free_pages
  ) ;
  let headroom_pages = Int64.sub actual_free_pages computed_free_pages in
  (* If XAPI overestimates there is this much extra space that can be used to
     cover for any underestimates.
     If this is negative then we can already run the system out of memory.
     *)
  Log.info (fun m -> m "Free pages headroom: %Ld" headroom_pages) ;
  if headroom_pages < 0L then
    Log.warn (fun m ->
        m "XAPI has already underestimated free memory: %Ld" headroom_pages
    ) ;

  let measure_overhead_pages value =
    V.set t ~vm value ;
    let pages =
      with_measure_memory_pages t ~host @@ fun () ->
      call t @@ VM.start_on ~host ~start_paused:true ~force:false ~vm
    in
    let memory_target_pages =
      call t @@ VM.get_memory_target ~self:vm |> bytes_to_pages
    and memory_overhead_pages_xapi =
      call t @@ VM.get_memory_overhead ~self:vm |> bytes_to_pages
    in
    call t @@ VM.hard_shutdown ~vm ;
    let overhead_pages = Int64.sub pages memory_target_pages in
    Log.debug (fun m ->
        m
          "Memory overhead %a %s = %Ld - %Ld = %Ld pages;@,\
           XAPI estimated = %Ld pages"
          V.pp value V.name pages memory_target_pages overhead_pages
          memory_overhead_pages_xapi
    ) ;
    let diff = Int64.sub overhead_pages memory_overhead_pages_xapi in
    let vms_required =
      if diff > 0L then begin
        let vms = div_round_up headroom_pages diff in
        Log.warn (fun m ->
            m
              "Memory overhead was underestimated by XAPI: %Ld < %Ld, diff: \
               %Ld.@,\
               VMs required for failure: %Ld"
              memory_overhead_pages_xapi overhead_pages diff vms
        ) ;
        vms
      end else
        Int64.max_int
    in
    (value, overhead_pages, vms_required)
  in
  Log.info (fun m -> m "Measuring VM %s impact on memory usage" V.name) ;
  let overhead_pages =
    V.values t ~host ~vm |> Seq.map measure_overhead_pages |> List.of_seq
  in
  match overhead_pages with
  | [] ->
      assert false
  | (x0, y0, vms0) :: rest ->
      let deltas =
        rest
        |> List.map @@ fun (x, y, vms) ->
           let x = V.to_int64 x and x0 = V.to_int64 x0 in
           let x = Int64.sub x x0 and y = Int64.sub y y0 in
           let ratio =
             if x > 0L then
               Int64.to_float y /. Int64.to_float x
             else
               0.
           in
           (x, y, ratio, vms)
      in
      Log.info (fun m -> m "%s,memory_overhead_pages,coeff,vms" V.name) ;
      let () =
        deltas
        |> List.iter @@ fun (x, y, r, vms) ->
           Log.info (fun m -> m "%Ld,%Ld,%g,%Ld" x y r vms)
      in
      let max_coeff =
        List.fold_left (fun rmax (_, _, r, _) -> Float.max rmax r) 0. deltas
      and min_vms =
        List.fold_left
          (fun rmin (_, _, _, vms) -> Int64.min rmin vms)
          vms0 deltas
      in
      let op, max_coeff_int =
        if Float.round max_coeff >= 1. then
          ("*", Float.ceil max_coeff |> Int64.of_float)
        else
          ("/", Float.floor (1. /. max_coeff) |> Int64.of_float)
      in
      Log.app (fun m ->
          m "VM memory_overhead_pages = ... + %s * %g =~ ... + %s %s %Ld" V.name
            max_coeff V.name op max_coeff_int
      ) ;
      if min_vms < Int64.max_int then begin
        Log.warn (fun m ->
            m "With %Ld VMs it might be possible to trigger OOM\n        error"
              min_vms
        ) ;
        let x, _, _ = overhead_pages |> List.rev |> List.hd in
        try_to_trigger_failure t ~host ~vm (module V) x (Int64.to_int min_vms)
      end
(*;
      max_coeff*)

module VCPU = struct
  type t = int64

  let to_int64 = Fun.id

  let name = "vcpu"

  let pp = Fmt.int64

  let set t ~vm value =
    call t @@ VM.set_VCPUs_max ~self:vm ~value ;
    call t @@ VM.set_VCPUs_at_startup ~self:vm ~value

  let values t ~host ~vm:_ =
    let limit =
      let host_cpus = call t @@ Host.get_host_CPUs ~self:host |> List.length in
      (* can't use more vCPUs than the host has, and we support a maximum of 64 *)
      min 64 host_cpus |> Int64.of_int
    in
    points_between 1L limit
end

module Pagetables = struct
  type t = MiB of int64

  let name = "pagetables"

  let to_bytes (MiB mib) = Int64.shift_left mib 20

  let to_int64 t =
    let ( ++ ) = Int64.add in
    let pages = Int64.div (to_bytes t) pagesize in
    (* pagetables for ranges of 2MiB, 1GiB, 512GiB, 256TiB, see
       https://wiki.osdev.org/Page_Tables#48-bit_virtual_address_space *)
    let pt = div_round_up pages 512L in
    let pd = div_round_up pt 512L in
    let pdp = div_round_up pd 512L in
    let pml4 = div_round_up pdp 512L in
    (* for future-proofing, we don't actually support this much memory *)
    let pml5 = div_round_up pml4 512L in
    (* Assuming that you'd have to allocate as 1GiB pages initially,
       then shatter them to 2MiB pages, and then finally to 4KiB pages.
     *)
    Int64.(pt ++ mul 2L pd ++ mul 3L pdp ++ mul 3L pml4 ++ mul 3L pml5)

  let of_bytes bytes = MiB (Int64.shift_right bytes 20)

  let set t ~vm mib = call t @@ VM.set_memory ~self:vm ~value:(to_bytes mib)

  let pp ppf (MiB mib as t) =
    Fmt.pf ppf "%Ld pagetables (for %Ld pages = %Ld MiB)" (to_int64 t)
      (Int64.div (to_bytes t) pagesize)
      mib

  let values t ~host ~vm =
    let total = call t @@ Host.compute_free_memory ~host in
    let value_max =
      call t @@ VM.maximise_memory ~total ~approximate:false ~self:vm
    in
    let start =
      [27; 28; 29; 30; 31; 39; 40]
      |> List.to_seq
      |> Seq.map (Int64.shift_left 1L)
      |> Seq.filter (fun v -> v < value_max)
    in
    Seq.append start Seq.(return value_max) |> Seq.map of_bytes
end

let specialise (name, speed, test) (module V : Variable) =
  let name = Printf.sprintf "%s: %s" name V.name in
  (name, speed, test (module V : Variable))

let variables = [(module VCPU : Variable); (module Pagetables : Variable)]

let tests () =
  let open Qt_filter in
  [
(*    ("Fill mem 1VM", `Slow, boot1)*)
  ("VM memory overhead", `Slow, calibrate)
  ; ("Fill mem 1VM (repeat)", `Slow, boot1)
  ]
  |> conn
  |> vm_template Qt.VM.Template.other
  |> List.concat_map (fun tc -> List.map (specialise tc) variables)
