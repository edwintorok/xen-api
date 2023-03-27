(** XAPI metadata service *)

type ('a, 'e) result = ('a, [> Rresult.R.msg | Rresult.R.exn_trap] as 'e) Result.t
(** result type containing an error message or an exception *)

val set_state: instance:Id.t -> Config.t option -> (unit, _) result
(** [set_state ~instnace desired] will set the state of the local metadata service
    [instance] to [desired].

    @param desired when [None] will stop any running service, when [Some
    config] will start, or reload/restart the service as needed to reach the
    [desired] state.
*)
