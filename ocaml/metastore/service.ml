open Types
open Errors

let src = Logs.Src.create __MODULE__

include (val Logs.src_log src)

(** [trap_exn2 f a b] calls [f a b] and traps any exceptions into a result type
    together with a backtrace
 *)
let trap_exn2 f v1 v2 = Rresult.R.trap_exn (fun (v1, v2) -> f v1 v2) (v1, v2)

(** [recover use result] calls [use] with the error value of [result] on error.
    @returns [result] or the return value of [use]
 *)
let recover use = Rresult.R.kignore_error ~use

module Make (L : Lifecycle) = struct
  type id_kind = L.id_kind
  type id = L.id
  type config = L.config

  let ( let* ) = Result.bind

  let task_await t = Rresult.R.trap_exn Vtasks.await_exn t

  let ( let+ ) vtask f = Result.bind (Result.bind vtask task_await) f

  (** [fdebug id name k] calls [k] when the debug log-level is enabled,
      similarly to {!Logs.debug}.
      It prefixes all log messages with [id(name)].
  *)
  let fdebug id func k =
    debug @@ fun m ->
    k @@ fun fmt -> m ("%s(%a): " ^^ fmt) func Id.dump id

  (* Error recovery *)

  (** [try_strategies id v [(name_1,f_1); ...; (name_n,f_n)]]
    tries [f_1 id v], and if it results in error tries the next strategy until
    [f_n id v].
    At each step it logs the attempt and any failures.
    Returns the result of the last strategy tried.
  *)
  let try_strategies id v =
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    let exec (name, f) =
      fdebug (fun m -> m "Trying %s" name) ;
      f id v
    in
    function
    | [] ->
        Fmt.error_msg "No strategies to try"
    | ((first_name, _) as first) :: rest ->
        let init = (first_name, exec first) in
        let f (prev_name, prev_result) ((next_name, _) as next) =
          ( next_name
          , prev_result
            |> recover @@ fun err ->
               fdebug (fun m -> m "%s failed: %a" prev_name pp_error err) ;
               fdebug (fun m -> m "Recovering, trying %s" next_name) ;
               exec next
          )
        in
        rest |> ListLabels.fold_left ~init ~f |> snd |> Errors.open_error

  let hexdigest_of_config instance =
    instance
    |> Fmt.to_to_string Instance.dump
    |> Digest.string
    |> Digest.to_hex

  let pp_instance_config =
    let pp ppf config =
      Fmt.pf ppf "running(config digest %s)" (hexdigest_of_config config)
    in
    Fmt.option ~none:Fmt.(any "stopped") pp

  let pp_state = Fmt.Dump.result ~ok:pp_instance_config ~error:pp_error

  (* Storing and retrieving running configuration *)

  let is_running = Rresult.R.trap_exn L.is_running_exn

  let get_config id =
    trap_exn2 Running_config.get_exn id

  let set_config id config =
    trap_exn2
      Running_config.set_exn id
      config

  let get_state id =
    let+ running = is_running id in
    let* config_opt = get_config id L.typ_of_config in
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    match (running, config_opt) with
    | true, Some config ->
        Ok (Some (Instance.v L.typ_of_config id config))
    | true, None ->
        Fmt.error_msg "instance running, but cannot retrieve configuration"
    | false, None ->
        Ok None
    | false, Some _ ->
        fdebug (fun m -> m "Removing stale configuration") ;
        let* () = set_config id None in
        Ok None

  (* Stopping *)

  let stop id config =
    let instance = Instance.v L.typ_of_config id config in
    let+ () = Rresult.R.trap_exn L.stop_exn instance in
    set_config id None

  let force_stop id _ = stop id None

  let stop_or_force id conf =
    try_strategies id (Some conf) [("stop", stop); ("force stop", force_stop)]

  (* Starting *)

  let start id conf =
    let+ () = trap_exn2 L.start_exn id conf in
    set_config id (Some conf)

  (* Reloading *)

  let reload id (current_conf, next_conf) =
    let+ () =
      trap_exn2
        (fun id (current, next) -> L.reload_exn id ~current ~next)
        id (current, next)
    in
    set_config id (Some next_config)

  let stop_start id (running, desired) =
    let* () = stop_or_force id running in
    start id desired

  (* Changing state of a service (start, reload, stop). *)

  let set_state id config_opt =
    let fdebug fmt = fdebug id __FUNCTION__ fmt in
    (* do not fail immediately if we couldn't retrieve a state,
       we are still able to force stop without one *)
    let current_state = get_state id in

    fdebug (fun m ->
        m "Instance state %a, desired %a" pp_state current_state
          pp_instance_config config_opt
    ) ;

    match (current_state, config_opt) with
    | Ok None, None ->
        Ok ()
    | Ok (Some running), None ->
        stop_or_force id running
    | Error _, None ->
        stop id None
    | Ok None, Some desired ->
        start id desired
    | Ok (Some running), Some desired when ValidConfig.equal running desired ->
        Ok ()
    | Ok (Some running), Some desired ->
        try_strategies id (running, desired)
          [
            ("reload", reload)
          ; ("stop+start", stop_start)
          ; ("start old configuration", fun id (old, _) -> start id old)
          ]
    | Error err, Some _ ->
        Error err

  let health_check id =
    let* conf = get_state id in
    match conf with
    | None ->
        Fmt.error_msg "Instance not running"
    | Some conf ->
        let _, valid = ValidConfig.to_pair conf in
        trap_exn2 L.health_check_exn id valid |> Result.map Task.await_exn
end
