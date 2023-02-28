(** a service instance *)

(** a {!result} type compatible with {!Rresult.R}.
  On success it contains a type ['a],
  and on error a string error message wrapped with [`Msg]
*)
type 'a msg_result = ('a, Rresult.R.msg) result

type id = Uuidm.t (** unique identifier for service instance *)

type kv = string Astring.String.Map.t (** key -> value map *)

module type S = sig
  module Config : sig
    (** configuration for a service *)
    type t

    val to_dict : t -> kv
    (** [to_dict config] converts [config] into a key-value map. *)

    val of_dict : kv -> t msg_result
    (** [of_dict dict] converts [dict] into a {!t} configuration.

      @returns [Ok t] when configuration is successfully converted
      @returns [Error (`Msg reason)] when the configuration is not valid
    *)

    val equal : t -> t -> bool
    (** [equal a b] returns whether [a] and [b] are the same. *)

  end

  val name: string
  (** [name] is the name of this service. *)

  val validate_exn: id -> Config.t -> unit
  (** [validate_exn id config] checks the validity of [config].
    This is useful to avoid restarting a service with an invalid configuration
    that would prevent the service from starting again.
    Best effort: an implementation that always returns [unit] is a valid choice
    if no additional validation can be performed.

    @raises an exception if the configuration is not valid
  *)

  val start_exn: id -> Config.t -> unit
  (** [start_exn id config] starts an instance of the service with [config] as configuration.

    @raises an exception if the service could not be started
  *)

  val reload_exn: id -> Config.t -> bool
  (** [reload_exn id config] reloads the configuration for instance [id].
    If the service supports runtime reconfiguration it should attempt to change
    as many parameters as possible at runtime.

    @raises an exception if the service could not be reloaded

    @returns false if a reload is not possible in this situation. The caller
    should fall back to {!stop_exn} and {!start_exn} as needed.
  *)

  val stop_exn: id -> force:bool -> Config.t -> unit
  (** [stop_exn id ~force config] stops an instance of the service with [config] as configuration.

    @param force when true will immediately kill the service

    @raises an exception if the service could not be started
  *)

  val is_running_exn: id -> check_health:bool -> bool
  (** [is_running_exn id ~check_health] checks the running state of instance [id].

    @param check_health whether to also perform a health check

    @raises an exception if the service could not be started
  *)
end
