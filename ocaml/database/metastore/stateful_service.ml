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

module Make(A: FullAction) = struct
  type 'a config = 'a A.config
  type diagnostic = A.diagnostic

  let get_state = trap_exn2 A.monitor_exn

  (* low-level implementations *)

  (* the [id] parameter is required below to avoid weak type variables *)

  let start id = trap_exn2 A.start_exn id
  let stop id = trap_exn2 A.stop_exn id
  let reload id = trap_exn2 A.reload_exn id
  let validate id = trap_exn2 A.validate_exn id

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
