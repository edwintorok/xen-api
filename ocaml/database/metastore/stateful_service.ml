let src = Logs.Src.create __MODULE__
include (val Logs.src_log src)

open Types

(** [fdebug id name k] calls [k] when the debug log-level is enabled,
    similarly to {!Logs.debug}.
    It prefixes all log messages with [name].
*)
let fdebug id func k =
  debug @@ fun m ->
  k @@ fun fmt -> m ("%s(%s): " ^^ fmt) func @@ Id.to_string id

module Fallback(A: Action) = struct
  include A

  let validate_exn id _ =
    fdebug id __FUNCTION__ (fun m -> m "noop")

  let reload_exn id config =
    let fdebug = fdebug id __FUNCTION__ in
    let stop _ =
      fdebug (fun m -> m "Already running during reload, stopping");
      A.stop_exn id (Some config)
    in
    fdebug (fun m -> m "Checking state");
    A.monitor_exn id Readonly
    |> Option.iter stop;
    A.start_exn id config
end

(** [trap_exn2 f a b] calls [f a b] and traps any exceptions into a result type
    together with a backtrace
 *)
let trap_exn2 f v1 v2 = Rresult.R.trap_exn (fun (v1, v2) -> f v1 v2) (v1, v2)

let (let+) = Result.bind

let pp_error ppf = function
  | `Msg _ as m -> Rresult.R.pp_msg ppf m
  | `Exn_trap _ as e -> Rresult.R.pp_exn_trap ppf e

module Backoff = struct
  type config = { max_sleep: float; deadline: Mtime.t }
  type t = { config: config; count: int; sleep: float }

  (** [create ?max_sleep ?timeout ()] creates a truncated exponential backoff.

      @param max_sleep is the maximum amount to sleep between retries
      @param timeout is the time after which to give up completely
   *)
  let create ?(max_sleep = 1.0) ?(timeout = Mtime.Span.(60 * s)) ?(sleep = 0.1) () =
    match Mtime.add_span (Mtime_clock.now ()) timeout with
    | None -> invalid_arg "Mtime overflow"
    | Some deadline ->
        { config = { max_sleep; deadline }; count = 0; sleep }

  (** [backoff t] sleeps for a time determined by the backoff configuration and returns a new backoff configuration.
    The sleep includes a randomized delay to avoid correlated periodic actions.
  *)
  let backoff t =
    if Mtime.is_later (Mtime_clock.now ()) ~than:t.config.deadline then begin
      debug (fun m -> m "Timeout expired");
      None
    end else begin
      let next = { t with count = t.count + 1; sleep = min t.config.max_sleep (2. *. t.sleep) } in
      (* introduce some jitter *)
      Thread.delay (Random.float t.sleep);
      Some next
    end
end

let run_path = ref @@ Fpath.v "/var/run/nonpersistent/xapi"

module Make(A: FullAction) = struct
  type config = A.config
  let typ_of_config = A.typ_of_config

  type diagnostic = A.diagnostic
  let typ_of_diagnostic = A.typ_of_diagnostic

  let dump_config = Serialization.dump A.typ_of_config
  let dump_diagnostic = Serialization.dump A.typ_of_diagnostic
  let dump_metadata = Serialization.dump A.typ_of_metadata

  (* Configuration state management.
     State has to be stored on disk to survive XAPI restarts.
   *)
  let path_of_id id =
    Fpath.(!run_path / Id.to_string id)

  let load_config id =
    let+ contents =
        id |> path_of_id
      |> Rresult.R.trap_exn Serialization.string_of_file_exn
    in
    Serialization.deserialize typ_of_config contents

  let save_config id config =
    config |> Serialization.serialize typ_of_config
    |> Serialization.string_to_file_exn (path_of_id id)

  let save_config = trap_exn2 save_config

  (* get_state calls monitor, there is no additional validation that we could
     perform here in general (unit tests *can* perform additional validations
     where they carefully control concurrency)
  *)
  let get_state = trap_exn2 A.monitor_exn

  let get_state id level =
    let+ state = get_state id level in
    match state with
    | None -> Ok None
    | Some state ->
      (* config is loaded only when state was successfully queried. *)
      let+ config = load_config id in
      Ok (Some (state, config))

  (* low-level implementations *)

  (* the [id] parameter is required below to avoid weak type variables *)

  let start id = trap_exn2 A.start_exn id
  let stop id = trap_exn2 A.stop_exn id
  let reload id = trap_exn2 A.reload_exn id
  let validate id = trap_exn2 A.validate_exn id

  let is_known_same state config_opt =
    match state with
    | Ok c when Option.equal A.equal_config c config_opt ->
        true
    | Error _ | Ok _ ->
        (* an error is considered to be an unknown state, and thus we cannot
           say it is known equal to any other desired configuration *)
        false

  (* idempotent set_state with no validation *)
  let set_state id ~force config_opt =
    let fdebug = fdebug id __FUNCTION__ in
    (* always retrieve state, but don't fail if we can't, this is useful for
       low-level shutdown *)
    match get_state id Readonly, config_opt with
    | Ok None, None ->
        fdebug (fun m -> m "Service already stopped: no operation");
        Ok ()
    | Ok (Some (_, running_config)), Some config when A.equal_config running_config config ->
        fdebug (fun m -> m "Service state matches desired state: no operation");
        Ok ()
    | _, None when force -> stop id None
    | Ok (Some (_, running_config)), None -> stop id @@ Some running_config
    | Error e, _ ->
        warn (fun m -> m "%s: asked to change state gracefully, but cannot retrieve current state" (Id.to_string id));
        (* only way out would be to retry with a force stop, but make that the
           caller's choice *)
        Error e
    | Ok None, Some config ->
        fdebug (fun m -> m "Service not running, starting");
        start id config
    | Ok (Some (_, old)), Some next ->
        reload id (old, next)
    end

  (* add precondition validation *)
  let set_state id ~backoff =
    let fdebug = fdebug id __FUNCTION__ in
    function
    | None ->
        (* no input, nothing to validate *)
        set_state id ~backoff None
    | Some config as config_opt ->
      let+ () = validate id (config, Readonly) in
      fdebug (fun m -> m "Configuration validated");
      set_state id ~backoff config_opt

  (* add post-condition validation on successful returns *)
  let set_state id ~backoff config_opt =
    let fdebug = fdebug id __FUNCTION__ in
    let+ () = set_state id ~backoff config_opt in
    let+ state = get_state id Readonly in
    match config_opt, state with
    | None, None ->
        fdebug (fun m -> m "Service stopped as requested");
        Ok ()
    | Some _, Some _ ->
        fdebug (fun m -> m "Service running as requested");
        Ok ()
    | None, Some _ ->
        warn (fun m -> m "%s: Asked to stop, but still running" @@ Id.to_string id);
        Fmt.error_msg "Asked to stop but still running"
    | Some _, None ->
        (* could be a bug in start too, it must only return when service is
           running, but of course the service may crash again *)
        warn (fun m -> m "%s: Asked to start, but not running. Did it crash?" @@ Id.to_string id);
        Fmt.error_msg "Asked to start, but not running"

  (* add retries with backoff *)
  let set_state id config_opt =
    let fdebug = fdebug id __FUNCTION__ in
    let rec loop ~backoff =
      let config, perform_backoff = backoff in
      match set_state id ~backoff config_opt with
      | Ok () as ok -> ok
      | Error e ->
          fdebug (fun m -> m "Service state change failed, attempting to backoff: %a" pp_error e);
          match perform_backoff config with
          | None ->
              warn (fun m -> m "Backoff gave up (timeout)");
              Fmt.error_msg "Service state change timed out"
          | Some config ->
              loop ~backoff:(config, perform_backoff)
    in
    loop

  (* save and load old state *)

  (* try to restore old state upon failure.
     this requires get_state to return old state, and for set_state to save new
     state upon success, but what to do about intermediate states?
     Since we only use this for restore so reading it once at beginning should
     be fine
   *)


  (* higher-level implementations that perform additional validation *)
  let stop id config_opt =
    let fdebug = fdebug id __FUNCTION__ in
    let+ () = stop id config_opt in
    (* cannot use levels higher than Readonly here: might've started another
       service on same port, etc. *)
    let+ state = get_state id Readonly in
    match state with
    | None ->
        fdebug (fun m -> m "Service stopped");
        Ok ()
    | Some _ ->
        fdebug (fun m -> m "Service didn't stop");
        (* this is a bug in [stop], if it succeeds the service must not be
           running anymore! *)
        Fmt.error_msg "Service claimed to have stopped, but still running"

  let rec check_health id backoff =
    let fdebug = fdebug id __FUNCTION__ in
    let+ state = get_state id Readonly in
    match state with
    | None ->
      warn (fun m -> m "%s: Service start claims to have succeeded, but service not running. Did it crash?" @@ Id.to_string id);
      Fmt.error_msg "Start succeeded, but service not running"
    | Some _ ->
      match get_state id Active with
      | Ok _ ->
        fdebug (fun m -> m "Service is healthy");
        Ok ()
      | Error e ->
        (* active health check not required to succeed immediately, retry,
           do not log all the retries here though.
          We want these retries to be cancellable by
          [Task_helpers.exn_if_cancelling] though, so the health checker needs
          access to the config (which can include a __context).
          If we use a timeout here, that should probably be part of the
          [config] though!
           *)
        (* TODO: the retries should probably be done at a higher level?
           See Helpers.Policy.wait, but make it use mtime and our exponential
           backoff algo instead?
         *)
        match Backoff.backoff backoff with
        | None ->
          Fmt.error_msg "Timeout reached on health check, last failure: %a" pp_error e

        | Some backoff ->
          check_health id backoff

  let set_state id =
    let fdebug = fdebug id __FUNCTION__ in
    function
    | None ->
    (* TODO: retrieve old config? *)
      let config_opt = None in
      fdebug (fun m -> m "desired stop");
      (match stop id config_opt with
      | Error e when config_opt <> None ->
          warn (fun m -> m "Service failed to stop, forcing: %a" pp_error e);
          stop id None
      | r -> r
      )
    | Some config ->
      (* Always perform the minimal validation.
         Especially useful for reload so we don't take down the service
         completely with an invalid configuration
       *)
      let+ () = validate id (config, Readonly) in
      let+ () = match get_state id Readonly with
      | Ok None ->
          fdebug (fun m -> m "Not running, starting");
          (* only place where we can perform the active validation *)
          let+ () = validate id (config, Active) in
          start id config
      | Ok (Some _) ->
          (* at a high level we don't know whether the configuration has changed or not*)
          fdebug (fun m -> m "Already running, reloading");
          reload id config
      | Error err ->
          warn (fun m -> m "%s: want to set configuration, but current state unknown: %a" (Id.to_string id) pp_error err);
          (* we don't want to force stop here, since that might interrupt service,
             perhaps the state query is just a temporary failure: propagate failure *)
          Error err
      in
      check_health id @@ Backoff.create ()

end
