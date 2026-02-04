open Quicktest_api_helpers
open Client.Client
open Quicktest_trace
open Quicktest_trace_api
open Quicktest_trace_rpc

let check_tasks tasks =
  tasks |> List.map @@ function Ok x -> x | Error exn -> raise exn

let meminfo why xc scope =
  let mem = Xenctrlext.HostNuma.numa_get_meminfo xc in
  Scope.add_event scope (fun () ->
      let open Xenctrlext.HostNuma in
      let attrs =
        mem
        |> Array.to_seqi
        |> Seq.concat_map (fun (i, meminfo) ->
            let key name i64 =
              (Printf.sprintf "node%d_%s" i name, `Int (Int64.to_int i64))
            in
            List.to_seq
              [
                key "free" meminfo.free
              ; key "claimed" meminfo.claimed
              ; key "size" meminfo.size
              ; key "free-claimed" Int64.(sub meminfo.free meminfo.claimed)
              ; ("reason", `String why)
              ]
        )
        |> List.of_seq
      in
      Opentelemetry.Event.make "numa_meminfo" ~attrs
  ) ;
  Xenctrl.with_intf (fun xc -> Xenctrl.send_debug_keys xc "u") ;
  mem

let vm_set_numa_node f xc t ~vm =
  let topo = Xenctrlext.cputopoinfo xc in
  let cpus =
    topo
    |> Array.to_seqi
    |> Seq.filter_map (fun (i, t) ->
        if f t.Xenctrlext.node then
          Some (string_of_int i)
        else
          None
    )
    |> List.of_seq
  in
  Api.VM.call_set t VM.set_VCPUs_params ~self:vm
    ~value:[("mask", String.concat "," cpus)]

let div_round_up a b = Int64.(div (add a @@ pred b) b)

let mib = Int64.shift_left 1L 20

let numawalk_hard xc t ~fit ~host ~vm ~vms node =
  Trace.with_ "numawalk_hard" ~attrs:[("node", `Int node)] @@ fun scope ->
  let mem = meminfo "begin_one" xc scope in
  let vm_node = List.nth vms node in
  let mem_node = mem.(node) in
  let host_free = call t @@ Host.compute_free_memory ~host in
  let max_vm_memory =
    Api.VM.call_get t ~self:vm_node
    @@ Api.VM.maximise_memory ~total:host_free ~approximate:false
  in
  let value = Int64.sub mem_node.free mem_node.claimed in
  let value =
    if fit then
      Api.VM.call_get t ~self:vm_node
      @@ Api.VM.maximise_memory ~total:value ~approximate:false
    else
      (* need to round to workaround XAPI rounding bug in compute_overhead *)
      div_round_up value mib |> Int64.mul mib
  in
  (* if we only have 1 NUMA node avoid using more than XAPI thinks we have free
     *)
  let value = Int64.min max_vm_memory value in
  Api.VM.call_set t VM.set_memory ~self:vm_node ~value ;
  let overhead =
    Api.VM.with_call t "compute_memory_overhead" vm_node
    @@ VM.compute_memory_overhead ~vm:vm_node
  in

  let other_vms =
    Option.to_list
      (vms
      |> List.find_opt (( <> ) vm_node)
      |> Option.map @@ fun self ->
         let value = Int64.(sub host_free (add value overhead)) in
         let value =
           Api.VM.call_get t ~self
           @@ Api.VM.maximise_memory ~total:value ~approximate:false
         in
         Api.VM.call_set t VM.set_memory ~self ~value ;
         vm_set_numa_node (( <> ) node) xc t ~vm:self ;
         (host, self)
      )
  in

  (* start other first, otherwise Xen would protect node 0, and we won't be
     able to make it run out *)
  let () =
    Trace.with_ "start_other" ~scope @@ fun _ ->
    start_vms t other_vms ;
    let (_ : _ array) = meminfo "after_start_other" xc scope in
    ()
  in

  fill_mem_pow2 t ~host ~vm

  let () =
    Trace.with_ "start_1" ~scope @@ fun _ ->
    start_vm t ~host ~vm:vm_node ;
    let (_ : _ array) = meminfo "after_start_1" xc scope in
    ()
  in

  shutdown_vms t (List.map snd other_vms) ;

  (* Try to start a lot of VMs, if we have allocations that specifically only
     use node 0, then we should be able to trigger an OOM *)
  let free = call t @@ Host.compute_free_memory ~host in
  let static_min = Api.VM.call_get t VM.get_memory_static_min ~self:vm in
  let n = min (Int64.div free static_min) 64L |> Int64.to_int in

  let filled_vms =
    Trace.with_ "fill_remaining" ~scope @@ fun scope ->
    let (_ : _ array) = meminfo "before_fill_remaining" xc scope in
    let vms = fill_mem_n t ~host ~vm ~n in
    let (_ : _ array) = meminfo "after_fill_remaining" xc scope in
    let free = call t @@ Host.compute_free_memory ~host in
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make "host_memory_free"
          ~attrs:[("free_bytes", `Int (Int64.to_int free))]
    ) ;
    vms
  in
  Xenctrl.with_intf (fun xc -> Xenctrl.send_debug_keys xc "u") ;
  shutdown_vms t (vm_node :: List.map snd filled_vms)

(*
  (* start a big VM on each of the other NUMA nodes, such that the available
     memory on each NUMA node (other than node) is < static_min (but not 0).
     Because hard affinity doesn't actually guarantee all overhead goes onto
     that node too, we do need to fill all nodes first, and then shut down one
     VM, to ensure a more even spread.
     *)
  let block =
    mem
    |> Array.to_seqi
    |> Seq.filter_map (fun (i, info) ->
          (* TODO: also check distance for validity *)
          let open Xenctrlext.HostNuma in
          let self = List.nth vms i in
          let value =
            Api.VM.call_get t ~self
            @@ Api.VM.maximise_memory
                 ~total:Int64.(sub info.free info.claimed)
                 ~approximate:false
          and static_min = Api.VM.call_get t VM.get_memory_static_min ~self in
          let value = Int64.sub value Int64.(div static_min 2L) in
          if value < static_min then
            None
          else begin
            Api.VM.call_set t VM.set_memory ~self ~value ;
            vm_set_numa_node xc t ~vm:self i ;
            Some (host, self)
          end
    )
    |> List.of_seq
  in
  let () = Trace.with_ "start_vms_block" ~scope @@ fun _ -> start_vms t block in
  let node_vm = List.nth vms node in
  shutdown_vms t [node_vm];

  let static_min = Api.VM.call_get t VM.get_memory_static_min ~self:vm in
  let filled_vms =
    Trace.with_ "starts_vms_fill_one" ~scope @@ fun _scope ->
    let mem = (meminfo "begin_fill_one" xc scope).(node) in
    (* TODO: not more than the total says... *)
    let total = Int64.sub mem.free mem.claimed in
    let n = min (Int64.div total static_min) 32L |> Int64.to_int in
    let filled_vms = fill_mem_n t ~total ~host ~vm ~n in
(*    let vm = List.nth vms node in
    Api.VM.call_set t VM.set_memory ~self:vm ~value:total;
    let filled_vms = [host, vm] in
    start_vm t ~host ~vm;*)
    let (_ : _ array) = meminfo "after_fill_one" xc scope in
    filled_vms
  in

  (* we've started the VMs where we wanted, now free up memory on other nodes *)
  shutdown_vms t (List.map snd block) ;

  (* Try to start a lot of VMs, if we have allocations that specifically only
     use node 0, then we should be able to trigger an OOM *)
  let free = call t @@ Host.compute_free_memory ~host in
  let n = min (Int64.div free static_min) 64L |> Int64.to_int in

  let filled_vms2 =
    Trace.with_ "fill_remaining" ~scope @@ fun scope ->
    let (_ : _ array) = meminfo "before_fill_remaining" xc scope in
    let vms = fill_mem_n t ~host ~vm ~n in
    let (_ : _ array) = meminfo "after_fill_remaining" xc scope in
    let free = call t @@ Host.compute_free_memory ~host in
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make "host_memory_free"
          ~attrs:[("free_bytes", `Int (Int64.to_int free))]
    ) ;
    vms
  in
  shutdown_vms t (filled_vms @ filled_vms2 |> List.map snd)
  *)

let one t ~host ~vm =
  Trace.with_ __FUNCTION__ @@ fun _ ->
  let xc = Xenctrlext.get_handle () in
  let nodes = Xenctrlext.get_nr_nodes xc in
  let vms = ensure_vm_clones t ~vm nodes "numawalk_block" in
  for i = 0 to nodes - 1 do
    numawalk_hard xc t ~host ~vm ~vms i
  done

let test rpc session_id template () =
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm -> one t ~host ~vm

let tests () =
  let open Qt_filter in
  [
    [("VM memory tests", `Slow, test)]
    |> conn
    |> vm_template Qt.VM.Template.other
  ]
  |> List.concat
