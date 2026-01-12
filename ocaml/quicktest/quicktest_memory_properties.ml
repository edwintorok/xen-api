open Client.Client
open Xapi_client_util

module D = Debug.Make (struct let name = "quicktest_memory_properties" end)

let vm_ref rpc = Ref.t_of_rpc (fun _ -> `VM) rpc

let vm_clone t ~new_name vm = task t vm_ref @@ Async.VM.clone ~vm ~new_name

let vm_destroy t self = task t Rpc.unit_of_rpc @@ Async.VM.destroy ~self

let with_vm_clones t n vm f =
  let base = call t @@ VM.get_name_label ~self:vm in
  let clone t i =
    let new_name = Printf.sprintf "%s-clone-%d" base i in
    vm_clone t ~new_name vm
  in
  List.init n Fun.id |> with_objects_exn t clone vm_destroy f

let if_asserted t ~vm (assertf : unit api)
    (f : (vm:API.ref_VM -> API.ref_task) api) =
  match call t @@ assertf with
  | () ->
      Some (task t ignore @@ f ~vm)
  | exception Api_errors.Server_error _ ->
      None

let operation t ~host ~vm =
  let vm_task f = Some (task t ignore f)
  and unit_task f = Some (task t ignore f) in
  function
  | `snapshot ->
      let new_name = Printf.sprintf "snapshot-%f" (Unix.gettimeofday ()) in
      vm_task @@ Async.VM.snapshot ~new_name ~ignore_vdis:[] ~vm
  | `clone ->
      let new_name = Printf.sprintf "clone-%f" (Unix.gettimeofday ()) in
      vm_task @@ Async.VM.clone ~new_name ~vm
  | `copy ->
      let new_name = Printf.sprintf "copy-%f" (Unix.gettimeofday ()) in
      vm_task @@ Async.VM.copy ~new_name ~sr:Ref.null (* use existing *) ~vm
  | `revert -> (
    match call t @@ VM.get_snapshots ~self:vm with
    | [] ->
        None
    | snapshot :: _ ->
        unit_task @@ Async.VM.revert ~snapshot
  )
  | `checkpoint ->
      let new_name = Printf.sprintf "checkpoint-%f" (Unix.gettimeofday ()) in
      vm_task @@ Async.VM.checkpoint ~new_name ~vm
  | `snapshot_with_quiesce ->
      let new_name =
        Printf.sprintf "snapshot_with_quiesce-%f" (Unix.gettimeofday ())
      in
      vm_task @@ Async.VM.snapshot_with_quiesce ~new_name ~vm
  | `provision ->
      unit_task @@ Async.VM.provision ~vm
  | `start ->
      unit_task @@ Async.VM.start ~start_paused:false ~force:false ~vm
  | `start_on ->
      (* TODO: separate assert checking from performing, to increase
         concurrency *)
      if_asserted t ~vm VM.(assert_can_boot_here ~host ~self:vm)
      @@ Async.VM.start_on ~host ~start_paused:true ~force:false
  | `pause ->
      unit_task @@ Async.VM.pause ~vm
  | `unpause ->
      unit_task @@ Async.VM.unpause ~vm
  | `clean_shutdown ->
      unit_task @@ Async.VM.clean_shutdown ~vm
  | `clean_reboot ->
      unit_task @@ Async.VM.clean_reboot ~vm
  | `hard_shutdown ->
      unit_task @@ Async.VM.hard_shutdown ~vm
  | `power_state_reset ->
      (* marked as dangerous *)
      None
  | `hard_reboot ->
      unit_task @@ Async.VM.hard_reboot ~vm
  | `suspend ->
      unit_task @@ Async.VM.suspend ~vm
  | `csvm ->
      None
  | `resume ->
      unit_task @@ Async.VM.resume ~force:false ~start_paused:false ~vm
  | `resume_on ->
      if_asserted t ~vm VM.(assert_can_boot_here ~host ~self:vm)
      @@ Async.VM.resume_on ~host ~force:false ~start_paused:true
  | `pool_migrate ->
      let pif = call t @@ Host.get_management_interface ~host in
      let network = call t @@ PIF.get_network ~self:pif in
      let dest = call t @@ Host.migrate_receive ~host ~network ~options:[] in
      let options = [] in
      if_asserted t ~vm
        VM.(
          assert_can_migrate ~live:true ~dest ~vm ~vdi_map:[] ~vif_map:[]
            ~vgpu_map:[] ~options
        )
      @@ Async.VM.pool_migrate ~host ~options
  | `migrate_send ->
      None (* TODO: pick host *)
  | `assert_operation_valid
  | `get_boot_record
  | `send_sysrq
  | `send_trigger
  | `query_services
  | `call_plugin
  | `awaiting_memory_live
  | `changing_static_range
  | `changing_memory_limits
  | `create_template
  | `changing_shadow_memory
  | `changing_shadow_memory_live
  | `changing_VCPUs
  | `changing_NVRAM
  | `data_source_op
  | `reverting
  | `sysprep
  | `update_allowed_operations
  | `destroy ->
      None
  | `changing_memory_live
  | `changing_dynamic_range
  | `changing_VCPUs_live
  | `create_vtpm ->
      None
  | `make_into_template ->
      unit_task @@ Async.VM.set_is_default_template ~value:true ~vm
  | `import | `export ->
      None
  | `metadata_export ->
      None
  | `shutdown ->
      unit_task @@ Async.VM.shutdown ~vm

let todo t ~host vm =
  call t @@ VM.get_allowed_operations ~self:vm
  |> List.filter_map (operation t ~host ~vm)
