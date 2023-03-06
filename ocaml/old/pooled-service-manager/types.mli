open Errors

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

(** Service identifier *)
module type Id = sig
  (** globally unique instance id *)
  type t

  val compare : t -> t -> int
  (** [compare a b] is a total order on identifiers {!t} *)

  val dump : t Fmt.t
  (** [dump formatter id] prints a representation of [id] for debugging *)

  val to_string : t -> string
  (** [to_string id] converts [id] into a string *)
end

(** identifier and configuration modules *)
module type Common = sig
  module Id : Id

  module Task : sig
    (** type of a task returning values of type ['a] *)
    type 'a t

    val await_exn : 'a t -> 'a
    (** [await_exn task] wait until [task] completes.
        @raises an exception if the [task] has failed
     *)

    (*
    val map_all: (Id.t -> 'a t) Map.Make(Id).t -> ('a, exn) result Map.Make(Id).t
    (** [wait_for_all lst] is a more efficient way for waiting for a group of
        tasks *)
    no need for this 'a t doesn't have to be the real task, can also be
    something given to a thread/domain pool/etc.
    and then creating the task really will be ... fast...
  *)
  end

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

    val equal : t -> t -> bool
    (** [equal a b] compares the configurations [a] and [b] *)

    val dump : t Fmt.t
    (** [dump formatter t] prints a representation of [t] for debugging *)
  end
end

(** Service lifecycle operations *)
module type Lifecycle = sig
  include Common

  val is_running_exn : Id.t -> bool Task.t
  (** [is_running_exn id] determines whether the instance [id] is running. *)

  val health_check_exn : Id.t -> ValidConfig.t -> unit Task.t
  (** [health_check_exn id config] performs a health check on instance [id]
    with configuration [config].
  *)

  val start_exn : Id.t -> ValidConfig.t -> unit Task.t
  (** [start_exn id config] starts an instance of the service [id] with [config]
    on the current host.
    It joins the instance to the pool if necessary.
    When this function returns the service must be ready to accept requests.
  *)

  val reload_exn :
    Id.t -> current:ValidConfig.t -> next:ValidConfig.t -> unit Task.t
  (** [reload_exn id ~current ~next] performs runtime reconfiguration on service [id]
    on the current host, if the service is running and reconfiguration is
    supported.
      It will not cause pool membership changes.
      On a successful return the service must be running with the new configuration.
  *)

  val stop_exn : Id.t -> ValidConfig.t option -> unit Task.t
  (** [stop_exn id config_opt] stops the service instance [id] on the current host.
    On a successful return the service must not be running.
    It makes the member leave the pool first if necessary.

    @params config_opt when None a force stop is performed, otherwise a
      graceful stop (which may require communication with other pool members,
      hence the supplied configuration of the running service)
  *)
end

(** a higher level interface to a local or pooled service *)
module type StatefulService = sig
  include Common

  val get_state : Id.t -> (ValidConfig.t option, [> error]) result
  (** [get_state id] retrieves the current state of the instance [id]
    on the current host.
    @returns [Ok None] if the instance is stopped
    @returns [Ok (Some config)] if the instance is running with [config]
    @return [Error err] if we failed to retrieve status
  *)

  val set_state : Id.t -> Config.t option -> (unit, [> error]) result
  (** [set_state id desired_config] changes the state of instance [id] to
  [desired_config] on the current host.

    It will attempt to perform minimally disruptive changes:
      * invalid configurations will be rejected early where possible
      * configuration will be reloaded through runtime reconfiguration where
        possible
      * service will be stopped and started as needed
      * service stop will be forced if a regular stop fails

    @param desired_config when None the instance will be stopped
    @param desired_config when [Some config] the instance will be reloaded or
      started to match [config]

    @returns [Ok ()] when successful
    @returns [Error err] when changing state failed
  *)

  val health_check : Id.t -> (unit, [> error]) result
  (** [health_check id] checks the health of the service. *)
end

(** configuration for a pool member *)
module type MemberConfig = sig
  (** configuration local to a member *)
  module Local : Config

  (** configuration shared between all members, including the member list itself *)
  module Global : Config

  (** member configuration *)
  type t = Global.t * Local.t
end
