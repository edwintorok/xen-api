open Types
open Errors

module Make (L : Lifecycle) : sig
  val get_state : id -> (L.ValidConfig.t option, [> error]) result
  (** [get_state id] retrieves the current state of the instance [id]
    on the current host.
    @returns [Ok None] if the instance is stopped
    @returns [Ok (Some config)] if the instance is running with [config]
    @return [Error err] if we failed to retrieve status
  *)

  val set_state : id -> L.Config.t option -> (unit, [> error]) result
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
