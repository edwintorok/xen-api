type 'a msg_result = ('a, Rresult.R.msg) result
(** a {!result} type compatible with {!Rresult.R}.
  On success it contains a type ['a], and on error a string error message
  wrapped with `Msg
*)

type 'a msg_or_trap_result = ('a, [< Rresult.R.msg | Rresult.R.exn_trap])

module type Service = sig
  (* TODO: another functor that provides a way to diff/split the config
    into local/global, live updatable and not parts and implement
    start/stop/reload efficiently and safely.
  *)

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

  val name: string
  (** [name] is a human readable unique name for this service *)

  module Config: sig
    type t (** configuration for a service *)

    val to_dict: t -> (string * string) list
    (** [to_dict config] converts [config] into a list of key-value pairs with unique keys. *)

    val of_dict: (string * string) list -> t msg_result
    (** [of_dict dict] converts [dict] into a {!t} configuration.
      @returns [Ok t] when configuration is successfully converted
      @returns [Error (`Msg reason)] when the configuration is not valid
    *)
  end

  module Id: sig
    (** A host specific unique identifier for a service.
      It has to be possible to start multiple services with different {!t}.
      If the service only supports a single instance per host (singleton)
      then this would be a unit type.
      Otherwise this would typically be an [IP:port] pair
    *)

    type t (** a locally unique identifier for a service *)

    val equal: t -> t -> bool
    (** [equal a b] returns whether [a] and [b] are the same identifier. *)

    val pp: t Fmt.t
    (** [pp formatter t] pretty prints [t] on [formatter]. *)

    val of_config: Config.t -> t
    (** [of_config config] computes a unique {!t} from [config].
      It has to be possible to start multiple instances with different [t].
      However it is not possible to start multiple instances with same
      [t].
      If the service only supports a single instance per host then this would
      be `unit`.
    *)
  end

  (** {1:Lifecycle operations} *)

  val start_exn: Id.t -> Config.t -> unit
  (** [start_exn instance config] starts the service [instance] with [config]. Idempotent.
    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises an exception if starting the service failed
  *)

  val stop_exn: Id.t -> force:bool -> unit
  (** [stop_exn service ~force] stops the service [instance]. Idempotent.
    When [force] is false it will try a graceful stop first, and at the
    implementation's discretion fall back to a forced stop.
    When [force] is true it will immediately kill the service.

    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises an exception if stopping the service failed
  *)

  val reload_exn: Id.t -> Config.t -> unit
  (** [reload_exn service config] changes the configuration of [service] to match [config]
    at runtime.

    Prefer to use this instead of [stop] followed by [start] because it might
    tell you ahead of time if a configuration change is not possible, in which
    case you can keep running the old service.

    The caller will ensure that there are no other lifecycle operations running
    on [instance] at the same time on the same host.

    @raises Invalid_argument if the [config] cannot be changed.
  *)

  val is_running_exn: Id.t -> bool
  (** [is_running_exn service] determines the status of [service].
    @returns [true] if the [service] is running.

    @raises an exception if we cannot determine the state of the service
  *)

  val health_check_exn: Id.t -> string option
  (** [health_check_exn service] returns a diagnostic message about [service].
    It can use the service's own diagnostic tools to determine this.

    @returns None if everything is running correctly
    @returns (Some msg) if there is a problem with the running service

    @raises an exception if there is an error communicating with the service
  *)
end

(** A manager for a pooled service *)
module Make(Svc: Service) : sig
  val set_state: Uuidm.t -> Svc.Config.t option -> unit msg_or_trap_result
  (** [set_state uuid config_opt] achieves the desired [config] state on member [uuid].
    It does so in the least disruptive way possible, e.g. by first reloading
    the configuration live if possible, and only then restarting the services.

    @param config_opt where [None] means the desired state is stopped,
      or [Some running] where the desired state is a service running with
      [running] configuration

    @returns Ok () on success
    @returns Error (`Msg reason) if there as a failure, with a diagnostic message
    @returns Error (`Exp_trap (exn, bt)) if there was an internal error processing the request
  *)

  val get_state: Uuidm.t -> check_health:bool -> Svc.Config.t option msg_or_trap_result
  (** [get_state uuid] retrieves the configuration for instance [uuid].

    @param check_health when false it only performs a basic 'is it running?' check

    @returns Ok None if the service is stopped
    @returns Ok (Some config) if the service is running with [config]
    @returns Error (`Msg reason) if there as a failure, with a diagnostic message
    @returns Error (`Exp_trap (exn, bt)) if there was an internal error processing the request
  *)
end
