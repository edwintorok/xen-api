val src : Logs.src
(** [src] is a logging source. Can be used to enable/disable logging of
  exceptions/backtraces. *)

(** Wraps all Service calls with {!Rresult.R.trap_exn}.
  Logs all caught exceptions and backtraces at debug level, together with the
  input (uuid, config) to help debugging.
*)
module Make (Svc : Service.S) : Service.S with type Config.t = Svc.Config.t
