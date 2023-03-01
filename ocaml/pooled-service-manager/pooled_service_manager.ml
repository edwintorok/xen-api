open Pooled_service
open Service

let run_dir = ref (Fpath.v "/var/run/nonpersistent/pooled-services")

let trap_exn f = Rresult.R.trap_exn f ()

let ( let* ) = Result.bind

let recover f (result : _ msg_or_trap_result) : _ msg_or_trap_result =
  match result with Ok _ as r -> r | Error err -> f err

let pp_msg_or_trap ppf = function
  | `Msg msg ->
      Fmt.string ppf msg
  | `Exn_trap _ as trap ->
      Rresult.R.pp_exn_trap ppf trap

module Make (Svc : S) = struct
  let src = Logs.Src.create __MODULE__

  include (val Logs.src_log src)

  let fdebug uuid funct k =
    debug (fun m -> k (fun fmt -> m ("%s(%a): " ^^ fmt) funct Uuidm.pp uuid))

  let write_dict_to_file path dict =
    dict
    |> List.to_seq
    |> Seq.map (fun (k, v) -> Printf.sprintf "%S=%S" k v)
    |> List.of_seq
    |> String.concat "\n"
    |> Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600
         (Fpath.to_string path)

  let pair k v = (k, v)

  let read_dict_of_file path =
    Xapi_stdext_unix.Unixext.read_lines ~path:(Fpath.to_string path)
    |> List.to_seq
    |> Seq.map (fun line -> Scanf.sscanf line "%S=%S" pair)
    |> List.of_seq

  let get_run_dir () =
    let dir = !run_dir in
    debug (fun m ->
        m "Ensuring runtime state directory is present: %a" Fpath.pp dir
    ) ;
    Xapi_stdext_unix.Unixext.mkdir_rec (Fpath.to_string dir) 0o700 ;
    dir

  let todo = Fmt.error_msg "TODO"

  let get_state id ~check_health :
      Svc.state msg_or_trap_result * Svc.Config.t msg_or_trap_result =
    let state_path = Fpath.(get_run_dir () / Uuidm.to_string id) in
    let config = read_dict_of_file state_path |> Svc.Config.of_dict in

    (* TODO: Svc.health_check_exn id *)
    let state = trap_exn @@ fun () -> Svc.get_state_exn id in
    (state, config |> Rresult.R.open_error_msg)

  let stop_exn id current_state =
    let fdebug m = fdebug id __FUNCTION__ m in
    let open Svc in
    match current_state with
    | Ok Stopped, _ ->
        fdebug (fun m -> m "not running, and desired stop: no action needed.")
    | Ok (Stopping | Started), _ ->
        fdebug (fun m -> m "instance running, stopping") ;
        Svc.stop_exn ~force:false id
    | Ok Starting, _ ->
        (* could wait until it starts, but no point, maybe it hanged *)
        fdebug (fun m -> m "instance is starting, requested stop: killing.") ;
        Svc.stop_exn ~force:true id
    | Error err, _ ->
        fdebug (fun m ->
            m "instance in unknown state, desired stop: %a" pp_msg_or_trap err
        ) ;
        (* TODO: separate result for state, and config: we may be able to fetch
           config but not state *)
        Svc.stop_exn ~force:true id

  let ensure_started id current_state desired_config =
    let fdebug m = fdebug id __FUNCTION__ m in
    let open Svc in
    match current_state with
    | Ok Started, Ok current when Svc.Config.equal current desired_config ->
        fdebug (fun m -> m "already in desired state") ;
        Ok ()
    | Ok Starting, Ok current when Svc.Config.equal current desired_config ->
        fdebug (fun m -> m "service is starting with the desired configuration") ;
        Ok ()
    | Error err, Ok current ->
        fdebug (fun m ->
            m "Cannot determine current state, killing: %a" pp_msg_or_trap err
        ) ;
        trap_exn @@ fun () ->
        (* not quite right, configuration may have changed... *)
        (* TODO: separate result for state, and config: we may be able to fetch
           config but not state *)
        Svc.stop_exn ~force:true id ;
        fdebug (fun m -> m "Starting service") ;
        Svc.start_exn id desired_config
    | _, Error err ->
        trap_exn @@ fun () ->
        (* TODO: have the ID just be the uuid? but we can't check for conflicts then ... *)
        fdebug (fun m ->
            m
              "Cannot determine current configuration: %a. Killing based on \
               new ID"
              pp_msg_or_trap err
        ) ;
        Svc.stop_exn ~force:true id ;
        fdebug (fun m -> m "Starting service") ;
        Svc.start_exn id desired_config
    | Ok Stopped, _ ->
        trap_exn @@ fun () ->
        fdebug (fun m -> m "not running, starting") ;
        Svc.start_exn id desired_config
    | Ok (Stopping | Starting), Ok old ->
        trap_exn @@ fun () ->
        fdebug (fun m ->
            m
              "instance is stopping or starting with different config, but \
               desired start. Killing."
        ) ;
        Svc.stop_exn ~force:true id ;
        Svc.start_exn id desired_config
    | Ok Started, Ok current ->
        fdebug (fun m ->
            m "already running with different configuration: %a"
              Svc.Config.pp_dump current
        ) ;

        trap_exn (fun () -> Svc.reload_exn id desired_config)
        |> recover @@ fun err ->
           fdebug (fun m ->
               m "Reload failed (or not supported): %a" pp_msg_or_trap err
           ) ;
           trap_exn (fun () -> Svc.restart_exn id desired_config)

  let set_state uuid desired_state =
    let fdebug m = fdebug uuid __FUNCTION__ m in
    let state_path = Fpath.(get_run_dir () / Uuidm.to_string uuid) in
    fdebug (fun m -> m "querying state of instance") ;
    let current_state = get_state uuid ~check_health:false in
    match desired_state with
    | None ->
        trap_exn @@ fun () ->
        stop_exn uuid current_state ;
        fdebug (fun m ->
            m "removing instance state, stopped: %a" Fpath.pp state_path
        ) ;
        Sys.remove (Fpath.to_string state_path)
    | Some desired ->
        let dict = Svc.Config.to_dict desired in
        let* () = ensure_started uuid current_state desired in
        fdebug (fun m ->
            m "instance started with configuration %a" Svc.Config.pp_dump
              desired
        ) ;
        trap_exn @@ fun () -> write_dict_to_file state_path dict
end
