(** a {!result} type compatible with {!Rresult.R}.
  On success it contains a type ['a], and on error a string error message
  wrapped with `Msg
*)
type 'a msg_result = ('a, Rresult.R.msg) result

type 'a msg_or_trap_result = ('a, [Rresult.R.msg | Rresult.R.exn_trap]) result

(** service instance identifier *)
type id = Uuidm.t

module type S = sig
  (** A service that can be run on a pool of hosts.
  Manages global, local and runtime reload of configuration,
  starting/stopping services, and dependencies.


  Conventions:
  * functions that can raise exceptions should have `_exn` in their name

  * it is an implementation error for a function without `_exn` to raise an
  exception, although the Pooled Service Manager will try to handle and
  recover as best as it can from this situation.

  * functions that manipulate data structures should use result types instead of exceptions.

  * functions that manipulate system state should use exceptions. There are
  unavoidable errors outside of the application's control that we need to
  handle anyway, and exceptions create a stacktrace that makes it easier to
  debug them.
  *)

  type state =
    | Starting
    | Started
    | Stopping
    | Stopped
        (** service runtime state, a subset of [systemd] service states *)

  val name : string
  (** [name] is a human readable unique name for this service *)

  module Config : sig
    (** configuration for a service *)
    type t

    val to_dict : t -> (string * string) list
    (** [to_dict config] converts [config] into a list of key-value pairs with unique keys. *)

    val of_dict : (string * string) list -> t msg_result
    (** [of_dict dict] converts [dict] into a {!t} configuration.
      @returns [Ok t] when configuration is successfully converted
      @returns [Error (`Msg reason)] when the configuration is not valid
    *)

    val equal : t -> t -> bool
    (** [equal a b] returns whether [a] and [b] are the same. *)

    val pp_dump : t Fmt.t
    (** [pp_dump formatter t] prints [t] for debugging purposes. *)
  end

  (** {1:Lifecycle operations} *)

  val start_exn : id -> Config.t -> unit
  (** [start_exn instance config] starts the service [instance] with [config]. Idempotent.
    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises an exception if starting the service failed
  *)

  val stop_exn : id -> force:bool -> unit
  (** [stop_exn service ~force] stops the service [instance]. Idempotent.
    When [force] is false it will try a graceful stop first, and at the
    implementation's discretion fall back to a forced stop.
    When [force] is true it will immediately kill the service.

    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises an exception if stopping the service failed
  *)

  val restart_exn : id -> Config.t -> unit
  (** [restart_exn service config] restarts the service [instance] with [config].
    It is recommended to use this instead of [stop] followed by [start],
    because it can stop early if [config] is not valid in the current state.

    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises an exception if restarting the service failed, or [config] is not valid in the current state
  *)

  val reload_exn : id -> Config.t -> unit
  (** [reload_exn service config] changes the configuration of [service] to match [config]
    at runtime.

    Prefer to use this instead of [stop] followed by [start] because it might
    tell you ahead of time if a configuration change is not possible, in which
    case you can keep running the old service.

    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises Invalid_argument if the [config] cannot be changed.
  *)

  val get_state_exn : id -> state
  (** [get_state_exn service] determines the status of [service].

    @returns Starting if the service is being started, but hasn't fully started yet
    @returns Started if the service is running
    @returns Stopping if the service is being stopped
    @return Stopped if the service is not running

    @raises an exception if we cannot determine the state of the service
  *)

  val health_check_exn : id -> string option
  (** [health_check_exn service] returns a diagnostic message about [service].
    It can use the service's own diagnostic tools to determine this.

    @returns None if everything is running correctly
    @returns (Some msg) if there is a problem with the running service

    @raises an exception if there is an error communicating with the service
  *)
end

(* TODO: service dependencies, e.g. on IP address *)
