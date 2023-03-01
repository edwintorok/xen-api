(** a service instance *)

type id = Uuidm.t (** unique identifier for service instance *)

type kv = string Astring.String.Map.t (** key -> value map *)

module type S = sig
  (** {1:API errors}

    Follows the conventions from [Rresult].

    Raising exceptions should be avoided, however the
    pooled service manager will catch and handle any exceptions arising,
    using {!Trap_exn_service} functor where possible.

    @see https://ocaml.org/p/rresult/0.7.0/doc/Rresult/index.html#custom-error-types
  *)

  type error (** the type of service API errors *)

  type 'a result = ('a, [`Service of error]) Result.t
  (** service API results *)

  val pp_error: [`Service of error] Fmt.t
  (** [pp_error formatter error] pretty prints the API [error]. *)

  val open_error: 'a result -> ('a, [>`Service of error]) Result.t
  (** [open_error error] is a convenience function to make the polymorphic
    result type composable with other error types *)

  val error_to_msg: 'a result -> ('a, Rresult.R.msg) Result.t
  (** [error_to_msg result] converts the error in [result] to [`Msg reason]. *)

  module Config : sig
    (** configuration for a service *)
    type t

    val to_dict : t -> kv
    (** [to_dict config] converts [config] into a key-value map. *)

    val of_dict : kv -> t result
    (** [of_dict dict] converts [dict] into a {!t} configuration.

      @returns [Ok t] when configuration is successfully converted
      @returns [Error reason] when the configuration is not valid
    *)

    val equal : t -> t -> bool
    (** [equal a b] returns whether [a] and [b] are the same. *)
  end

  val name: string
  (** [name] is the name of this service. *)

  val validate: id -> Config.t -> unit result
  (** [validate id config] checks the validity of [config].
    This is useful to avoid restarting a service with an invalid configuration
    that would prevent the service from starting again.
    Best effort: an implementation that always returns [unit] is a valid choice
    if no additional validation can be performed.
  *)

  val start: id -> Config.t -> unit result
  (** [start id config] starts an instance of the service with [config] as configuration.
  *)

  val reload: id -> Config.t -> bool result
  (** [reload id config] reloads the configuration for instance [id].
    If the service supports runtime reconfiguration it should attempt to change
    as many parameters as possible at runtime.

    @returns false if a reload is not possible in this situation. The caller
    should fall back to {!stop_exn} and {!start_exn} as needed.
  *)

  val stop: id -> force:bool -> Config.t -> unit result
  (** [stop id ~force config] stops an instance of the service with [config] as configuration.

    @param force when true will immediately kill the service
  *)

  val is_running: id -> check_health:bool -> bool result
  (** [is_running id ~check_health] checks the running state of instance [id].

    @param check_health whether to also perform a health check
  *)
end
