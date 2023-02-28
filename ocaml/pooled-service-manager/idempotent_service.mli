open Service

type 'a trap_result = ('a, Rresult.R.exn_trap) result
(** is ['a] on success and [`Exn_trap (exn * backtrace)] on failure. *)

type 'a msg_or_trap_result = ('a, [Rresult.R.msg | Rresult.R.exn_trap]) result
(** a result containing ['a] on success, and a [`Msg reason], or
  or [`Exn_trap of (exn * backtrace)] on failure.

  [`Msg reason] is preferred when the error can be clearly explained based on
  input parameters
  [`Exn_trap ] is preferred when a stacktrace would be useful for debugging a
  failure.
*)

(** a service instance with idempotent operations *)
module Make(Svc: S) : sig
  (** All functions in this module are idempotent.
    They do not raise exceptions, but return a {!msg_result} or {!trap_result}.

  *)

  module Config = Svc.Config

  val validate: id -> Config.t -> unit trap_result
  (** [validate id config] validates [config] if possible. *)

  val start: id -> Config.t -> unit msg_or_trap_result
  (** [start id config] starts instance [id] with configuration [config].
    If instance is already running with [config] then no action is taken.
    It is an error if the instance is already running with a different
    configuration.
  *)

  val reload: id -> Config.t -> bool msg_or_trap_result
  (** [reload id config] reloads instance [id] with configuration [config].

    @returns [Ok false] if this is not supported or possible in the current situation
  *)

  val stop: id -> force:bool -> unit msg_or_trap_result
  (** [stop id ~force config] stops instance [id]. *)

  val state: id -> check_health:bool -> Config.t option trap_result
  (** [state id ~check_health] queries the state of instance [id].

    @param check_health when false only basic running status checks are
    performed

    @return Ok None if not running
    @return Ok (Some config) is running with [config]
    @return Error _ if the state could not be determined
  *)
end
