type error = [Rresult.R.msg | Rresult.R.exn_trap]

val equal_error : ([< error] as 'a) -> 'a -> bool
(** [equal_error a b] compares errors [a] and [b],
  useful for Result.equal *)

val open_error : ('a, error) result -> ('a, [> error]) result
(** [open_error result] makes the [result] error compatible with other polymorphic errors *)

val pp_error : error Fmt.t
(** [pp_error formatter error] pretty prints the [error]. *)

val error_to_msg : ('a, [< error]) result -> ('a, Rresult.R.msg) result
(** [error_to_msg result] converts errors in [result] to error messages. *)
