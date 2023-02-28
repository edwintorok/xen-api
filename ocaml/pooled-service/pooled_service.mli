type 'a msg_result = ('a, `Msg of string) result
(** a {!result} type compatible with {!Rresult.R}.
  On success it contains a type ['a], and on error a string error message
  wrapped with `Msg
*)

module type Service: sig
  (* TODO: another functor that provides a way to diff/split the config
    into local/global, live updatable and not parts and implement
    start/stop/reload efficiently and safely.
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
      @returns [Error (`Msg s)] when the configuration is not valid
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
    @returns [true] if the [service] is running

    @raises an exception if we cannot determine the state of the service
  *)
end
