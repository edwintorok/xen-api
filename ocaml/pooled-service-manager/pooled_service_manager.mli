open Pooled_service

(** A manager for a pooled service from a single host's point of view.

  A more generic version of this is available in XAPI that actually manages a
  pool of services.
*)
module Make (Svc : S) : sig
  val set_state : Uuidm.t -> Svc.Config.t option -> unit msg_or_trap_result
  (** [set_state instance config] achieves the desired [config] on the
  [instance].

  @param [config] when [None] will stop [instance] if already running
  @param [config] when [Some desired] will start [instance] with [desired] if
    not already running. Otherwise it will try to do a runtime reconfiguration
    for as many parameters as possible, falling back to a member restart if
      needed.
    It doesn't support rolling restarts, that is a higher level concept.

    @returns Ok () on success
    @returns Error (`Msg reason) if there as a failure, with a diagnostic message
    @returns Error (`Exp_trap (exn, bt)) if there was an internal error processing the request
  *)

  val get_state :
       Uuidm.t
    -> check_health:bool
    -> Svc.state msg_or_trap_result * Svc.Config.t msg_or_trap_result
  (** [get_state uuid ~check_health] retrieves the configuration for instance [uuid].

    @param check_health when false it only performs a basic 'is it running?' check

    @returns Ok None if the service is stopped
    @returns Ok (Some config) if the service is running with [config]
    @returns Error (`Msg reason) if there as a failure, with a diagnostic message
    @returns Error (`Exp_trap (exn, bt)) if there was an internal error processing the request
  *)
end
