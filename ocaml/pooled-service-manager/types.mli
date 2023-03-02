open Errors

(** unique identifier for an instance of a service *)
type id = Uuidm.t

(** key-value map for configuration *)
type kv = string Astring.String.Map.t

(** Service configuration *)
module type Config = sig
  (** service configuration *)
  type t

  val to_dict : t -> kv
  (** [to_dict t] converts [t] to a string map suitable for a configuration file. *)

  val of_dict : kv -> (t, [> error]) result
  (** [of_dict kv] parses the configuration from [kv].

    @returns [Ok t] if a syntactically valid configuration got parsed
    @returns [Error reason] if the configuration is not syntactically valid
  *)
end

(** Service lifecycle operations *)
module type Lifecycle = sig
  module Config : Config

  module ValidConfig : sig
    (** service configuration that has passed some validation *)
    type t

    val of_config : Config.t -> (t, [> error]) result
    (** [of_config config] validates the [config] as a whole,
      e.g. checks for missing fields or conflicting values.
      Although [config] should be syntactically correct by construction,
      some validation can only be performed on the configuration as a whole,
      which can only be done once all its fields have been set.

      @returns [Ok t] when the configuration is valid
      @returns [Error reason] when the configuration is not valid due to [reason]
    *)

    val to_config : t -> Config.t
    (** [to_config config] gives back the underlying config *)

    val equal : t -> t -> bool
    (** [equal a b] compares the configurations [a] and [b] *)
  end

  val is_running_exn : id -> bool
  (** [is_running_exn id] determines whether the instance [id] is running. *)

  val health_check_exn : id -> ValidConfig.t -> unit
  (** [health_check_exn id config] performs a health check on instance [id]
    with configuration [config].
  *)

  val start_exn : id -> ValidConfig.t -> unit
  (** [start_exn id config] starts an instance of the service [id] with [config]
    on the current host.
    When this function returns the service must be ready to accept requests.
  *)

  val reload_exn : id -> current:ValidConfig.t -> next:ValidConfig.t -> unit
  (** [reload_exn id ~current ~next] performs runtime reconfiguration on service [id]
    on the current host, if the service is running and reconfiguration is
    supported.
      On a successful return the service must be running with the new configuration.
  *)

  val stop_exn : id -> ValidConfig.t option -> unit
  (** [stop_exn id config_opt] stops the service instance [id] on the current host.
    On a successful return the service must not be running.

    @params config_opt when None a force stop is performed, otherwise a
      graceful stop (which may require communication with other pool members,
      hence the supplied configuration of the running service)
  *)
end
