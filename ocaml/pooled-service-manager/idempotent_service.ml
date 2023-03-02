open Types

let ( let* ) = Result.bind

let run_dir = ref (Fpath.v "/var/run/nonpersistent/pooled-service")

let get_run_dir_exn () =
  let dir = !run_dir in
  Xapi_stdext_unix.Unixext.mkdir_rec (Fpath.to_string dir) 0700 ;
  dir

let state_path_exn id =
  let dir = get_run_dir_exn () in
  Fpath.(dir / Uuidm.to_string id)

(* TODO: add debug *)

let write_dict id dict =
  let path = state_path_exn id in
  dict
  |> Astring.String.Map.to_seq
  |> Seq.map (fun (k, v) -> Printf.sprintf "%S=%S" k v)
  |> List.of_seq
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600
       (Fpath.to_string path)

let write_dict id dict = Rresult.R.trap_exn (fun () -> write_dict id dict) ()

let pair k v = (k, v)

let read_dict id =
  let path = state_path_exn id in
  Xapi_stdext_unix.Unixext.read_lines ~path:(Fpath.to_string path)
  |> List.to_seq
  |> Seq.map (fun line -> Scanf.sscanf line "%S=%S" pair)
  |> Astring.String.Map.of_seq

let remove_dict id =
  let path = state_path_exn id in
  Rresult.R.trap_exn Sys.remove (Fpath.to_string path)

let read_dict = Rresult.R.trap_exn read_dict

module Make (Svc : Service) = struct
  (* do not trust that Svc will trap all errors, trap them explicitly here *)
  module Svc = Trap_exn_service.Make (Svc)

  let src = Logs.Src.create __MODULE__

  include (val Logs.src_log src)

  module Config = Svc.Config

  let validate = Svc.validate

  let fdebug id funct k =
    debug (fun m -> k (fun fmt -> m ("%s(%a): " ^^ fmt) funct Uuidm.pp id))

  let get_running_config id =
    let* running = Svc.is_running id ~check_health:false in
    if running then
      let* dict = read_dict id in
      Config.of_dict dict |> Result.map Option.some
    else
      Ok None

  let pp_config = Fmt.using Config.to_dict Astring.String.Map.dump_string_map

  let name = Svc.name

  let is_running = Svc.is_running

  let start id desired =
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    let dict = Config.to_dict desired in
    match get_running_config id with
    | Ok (Some current) when Config.equal current desired ->
        fdebug (fun m -> m "instance already running, nothing to do") ;
        Ok ()
    | Ok (Some current) ->
        fdebug (fun m ->
            m "instance already running with different configuration. %a != %a"
              pp_config current pp_config desired
        ) ;
        Error (`Running_with_other_config (Config.to_dict current))
    | (Ok None | Error _) as r -> (
        let () =
          r
          |> Result.iter_error @@ fun err ->
             (* even if we cannot get the running configuration, attempt to start it *)
             fdebug (fun m ->
                 m "Cannot get running configuration: %a" pp_error err
             )
        in
        fdebug (fun m -> m "instance not running, starting") ;
        let* () = Svc.start id desired in
        let* () = write_dict id dict in
        let* running_config = get_running_config id in
        match running_config with
        | Some config when Config.equal config desired ->
            fdebug (fun m -> m "started") ;
            Ok ()
        | Some config ->
            fdebug (fun m ->
                m "configuration mismatch: %a != %a" pp_config config pp_config
                  desired
            ) ;
            Fmt.error_msg "instance %a started, but configuration doesn't match"
              Uuidm.pp id
        | None ->
            fdebug (fun m -> m "instance not running") ;
            Fmt.error_msg "instance %a started, but not running" Uuidm.pp id
      )

  let stop id config_opt =
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    let* () =
      match config_opt with
      | None ->
          fdebug (fun m -> m "force stop") ;
          Svc.stop id None
      | Some _ -> (
          fdebug (fun m -> m "graceful stop") ;
          match get_running_config id with
          | Ok config ->
              Svc.stop id config
          | Error e ->
              fdebug (fun m ->
                  m "failed to get running config, will force stop: %a" pp_error
                    e
              ) ;
              Svc.stop id None
        )
    in
    let* () = remove_dict id in
    let* running = Svc.is_running id ~check_health:false in
    if running then
      Fmt.error_msg "instance %a still running" Uuidm.pp id
    else
      Ok ()

  let reload id config =
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    match get_running_config id with
    | Ok (Some running) when Config.equal running config ->
        fdebug (fun m -> m "Already running with desired configuration") ;
        Ok ()
    | Ok (Some _) ->
        fdebug (fun m -> m "Running configuration is different, reloading") ;
        Svc.reload id config
    | Ok None ->
        fdebug (fun m -> m "Asked to reload, but not running. Starting") ;
        Svc.start id config
    | Error _ as err ->
        fdebug (fun m -> m "Failed to get running configuration for reload") ;
        (* caller will have to retry with stop + start *)
        err
end
