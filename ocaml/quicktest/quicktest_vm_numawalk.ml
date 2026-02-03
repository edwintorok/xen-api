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

let numawalk xc t ~host ~vms i =
  let vm0 = List.nth vms 0 and vm1 = List.nth vms 1 and vm2 = List.nth vms 2 in
  Trace.with_ "numawalk" ~attrs:[("node", `Int i)] @@ fun scope ->
  let mem = (meminfo xc scope).(i) in
  let available = Int64.sub mem.free mem.claimed in
  (* start one that'd use up a full NUMA node, although we can't predict which,
     TODO: could use hard affinity
   *)
  let value =
    Api.VM.call_get t ~self:vm2
    @@ Api.VM.maximise_memory ~total:available ~approximate:false
  in
  Api.VM.call_set t VM.set_memory ~self:vm0 ~value ;
  let value = Api.VM.call_get t VM.get_memory_static_min ~self:vm1 in
  Api.VM.call_set t VM.set_memory ~self:vm1 ~value ;

  start_vm t ~host ~vm:vm0 ;

  let (_ : _ array) = meminfo xc scope in
  (* now start another small *)
  start_vm t ~host ~vm:vm1 ;
  let (_ : _ array) = meminfo xc scope in

  let available = Int64.mul (pagesize ()) (localhost_free_pages scope) in
  let value =
    Api.VM.call_get t ~self:vm2
    @@ Api.VM.maximise_memory ~total:available ~approximate:false
  in
  Api.VM.call_set t VM.set_memory ~self:vm1 ~value ;
  start_vm t ~host ~vm:vm2 ;
  let (_ : _ array) = meminfo xc scope in
  () ;
  shutdown_vms t [vm0; vm1; vm2]

let one t ~host ~vm =
  Trace.with_ __FUNCTION__ @@ fun _ ->
  let xc = Xenctrlext.get_handle () in
  let nodes = Xenctrlext.get_nr_nodes xc in
  let vms = ensure_vm_clones t ~vm 3 "numawalk" in
  for i = 0 to nodes - 1 do
    numawalk xc t ~host ~vms i
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
