(** Various types and module types used by services *)

(** {1:Type aliases} *)

(** unique identifier for service instance *)
type id = Uuidm.t

(** key -> value map *)
type kv = string Astring.String.Map.t

(** {1:API errors}

  Follows the conventions from [Rresult].

  Raising exceptions should be avoided, however the
  pooled service manager will catch and handle any exceptions arising.

  @see https://ocaml.org/p/rresult/0.7.0/doc/Rresult/index.html#custom-error-types
*)

(** an error in the service configuration *)
type config_invalid_argument = {key: string; value: string; reason: string}

(** @see {!config_invalid_argument} *)
type config_error = [`Config_error of config_invalid_argument]

(** generic error *)
type generic_error = [Rresult.R.msg | Rresult.R.exn_trap]

(** an error starting or stopping the service *)
type lifecycle_error =
  [config_error | `Running_with_other_config of kv | generic_error]

(** reload not supported due to [reason] *)
type reload_error = [`Reload_not_supported of string]

(** service running but healthcheck failed *)
type healthcheck_error = [`Healthcheck_error of string]

(** all errors that can be returned by a service *)
type error = [reload_error | healthcheck_error | lifecycle_error]

val pp_error : error Fmt.t
(** [pp_error formatter error] pretty prints the API [error]. *)

val error_to_msg : ('a, error) result -> ('a, Rresult.R.msg) Result.t
(** [error_to_msg result] converts the error in [result] to [`Msg reason]. *)

type ('a, 'e) result = ('a, ([> error] as 'e)) Result.t

(** {1:Configuration} *)

module type Config = sig
  (** configuration for a service *)
  type t

  val to_dict : t -> kv
  (** [to_dict config] converts [config] into a key-value map. *)

  val of_dict : kv -> (t, [> config_error]) result
  (** [of_dict dict] converts [dict] into a {!t} configuration.

    @returns [Ok t] when the configuration is successfully converted
    @returns [Error reason] when the configuration is not valid
  *)

  val equal : t -> t -> bool
  (** [equal a b] returns whether [a] and [b] are the same. *)
end

(** {1:Service} *)

module type Service = sig
  module Config: Config

  val name : string
  (** [name] is the name of this service. *)

  val validate : id -> Config.t -> (unit, [> config_error]) result
  (** [validate id config] checks the validity of [config].
    This is useful to avoid restarting a service with an invalid configuration
    that would prevent the service from starting again.
    Best effort: an implementation that always returns [unit] is a valid choice
    if no additional validation can be performed.
  *)

  val start : id -> Config.t -> (unit, [> lifecycle_error]) result
  (** [start id config] starts an instance of the service with [config] as configuration.
  *)

  val reload :
    id -> Config.t -> (unit, [> reload_error | lifecycle_error]) result
  (** [reload id config] reloads the configuration for instance [id].
    If the service supports runtime reconfiguration it should attempt to change
    as many parameters as possible at runtime.

    @returns false if a reload is not possible in this situation. The caller
    should fall back to {!stop_exn} and {!start_exn} as needed.
  *)

  val stop : id -> Config.t option -> (unit, [> lifecycle_error]) result
  (** [stop id config] stops an instance of the service.

    @param config when [None] will force stop the service, otherwise will try to
      gracefully stop the service, potentially notifying other pool members first.
  *)

  val is_running :
       id
    -> check_health:bool
    -> (bool, [> healthcheck_error | generic_error]) result
  (** [is_running id ~check_health] checks the running state of instance [id].

    @param check_health whether to also perform a health check
  *)
end
