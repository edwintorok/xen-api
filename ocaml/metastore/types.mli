(** a generic configuration type *)
type 'a config = 'a Rpc.Types.typ * 'a

type 'a outcome = 'a Vtasks.t

module type Lifecycle = sig
  type id_kind (** type parameter for Id.t *)

  type id = id_kind Id.t (** unique identifier *)

  type config (** instance configuration type *)

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
