open Quicktest_api_helpers
open Client.Client
open Quicktest_trace
open Quicktest_trace_api
open Quicktest_trace_rpc

let check_tasks tasks =
  tasks |> List.map @@ function Ok x -> x | Error exn -> raise exn

let meminfo xc scope =
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
              ]
        )
        |> List.of_seq
      in
      Opentelemetry.Event.make "numa_meminfo" ~attrs
  ) ;
  mem

let vm_set_numa_node xc t ~vm node =
  let topo = Xenctrlext.cputopoinfo xc in
  let cpus =
    topo
    |> Array.to_seqi
    |> Seq.filter_map (fun (i, t) ->
        if t.Xenctrlext.node = node then
          Some (string_of_int i)
        else
          None
    )
    |> List.of_seq
  in
  Api.VM.call_set t VM.set_VCPUs_params ~self:vm
    ~value:[("mask", String.concat "," cpus)]

let numawalk xc t ~host ~vm ~vms node =
  Trace.with_ "numawalk" ~attrs:[("node", `Int node)] @@ fun scope ->
  let mem = meminfo xc scope in

  (* start a big VM on each of the other NUMA nodes, such that the available
     memory on each NUMA node (other than node) is < static_min (but not 0). *)
  let block =
    mem
    |> Array.to_seqi
    |> Seq.filter_map (fun (i, info) ->
        if i = node then
          None
        else
          let open Xenctrlext.HostNuma in
          let self = List.nth vms i in
          let value =
            Api.VM.call_get t ~self
            @@ Api.VM.maximise_memory
                 ~total:Int64.(sub info.free info.claimed)
                 ~approximate:false
          and static_min_half =
            Int64.div (Api.VM.call_get t VM.get_memory_static_min ~self) 2L
          in
          Api.VM.call_set t VM.set_memory ~self
            ~value:Int64.(sub value static_min_half) ;
          vm_set_numa_node xc t ~vm:self i ;
          Some (host, self)
    )
    |> List.of_seq
  in
  let () = Trace.with_ "start_vms_block" ~scope @@ fun _ -> start_vms t block in
  let mem = (meminfo xc scope).(node) in
  let total = Int64.sub mem.free mem.claimed in
  let filled = fill_mem_pow2' t ~total ~host ~vm in
  let self = List.nth vms node in
  let value = Api.VM.call_get t VM.get_memory_static_min ~self in
  Api.VM.call_set t VM.set_memory ~self ~value ;
  let () =
    Trace.with_ "start_when_node_full" ~scope @@ fun scope ->
    let (_ : _ array) = meminfo xc scope in
    start_vm t ~host ~vm:self
  in
  let (_ : _ array) = meminfo xc scope in
  shutdown_vms t (vms @ filled)

let one t ~host ~vm =
  Trace.with_ __FUNCTION__ @@ fun _ ->
  let xc = Xenctrlext.get_handle () in
  let nodes = Xenctrlext.get_nr_nodes xc in
  let vms = ensure_vm_clones t ~vm nodes "numawalk_block" in
  for i = 0 to nodes - 1 do
    numawalk xc t ~host ~vm ~vms i
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
