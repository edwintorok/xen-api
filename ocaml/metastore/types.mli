open Errors

(** a generic configuration type *)
type 'a config = 'a Rpc.Types.typ * 'a

type 'a outcome = 'a Vtasks.t

module type Common = sig
  type id_kind (** type parameter for Id.t *)

  type id = id_kind Id.t (** unique identifier *)


  (* TODO: module config after all? *)

  type config (** instance configuration type *)

  val typ_of_config: config Serializable.typ

end

module type Lifecycle = sig
  include Common

  val is_running_exn : id -> bool outcome
  (** [is_running_exn id] determines whether the instance [id] is running. *)

  val health_check_exn : (id, config) Instance.t -> unit outcome
  (** [health_check_exn id config] performs a health check on instance [id]
    with configuration [config].
  *)

  val start_exn : (id, config) Instance.t -> unit outcome
  (** [start_exn id config] starts an instance of the service [id] with [config]
    on the current host.
    It joins the instance to the pool if necessary.
    When this function returns the service must be ready to accept requests.
  *)

  val reload_exn :
    (id, config) Instance.t -> next:config -> unit outcome
  (** [reload_exn id ~current ~next] performs runtime reconfiguration on service [id]
    on the current host, if the service is running and reconfiguration is
    supported.
      It will not cause pool membership changes.
      On a successful return the service must be running with the new configuration.
  *)

  val stop_exn : (id, config option) Instance.t -> unit outcome
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

  val get_state : id -> ((id, config) Instance.t option, [> error]) result
  (** [get_state id] retrieves the current state of the instance [id]
    on the current host.
    @returns [Ok None] if the instance is stopped
    @returns [Ok (Some config)] if the instance is running with [config]
    @return [Error err] if we failed to retrieve status
  *)

  val set_state : id -> config option -> (unit, [> error]) result
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

  val health_check : id -> (unit, [> error]) result
  (** [health_check id] checks the health of the service. *)
end

(** configuration for a pool member *)
module type MemberConfig = sig
  type local (** configuration local to a member *)

  type global
  (** configuration shared between all members, including the member list itself *)

  (** member configuration *)
  type t = global * local
end
