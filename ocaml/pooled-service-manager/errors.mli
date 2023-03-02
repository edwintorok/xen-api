type error = [Rresult.R.msg | Rresult.R.exn_trap]

val open_error : ('a, error) result -> ('a, [> error]) result
(** [open_error result] makes the [result] error compatible with other polymorphic errors *)

val pp_error : error Fmt.t
(** [pp_error formatter error] pretty prints the [error]. *)

val error_to_msg : ('a, [< error]) result -> ('a, Rresult.R.msg) result
(** [error_to_msg result] converts errors in [result] to error messages. *)
