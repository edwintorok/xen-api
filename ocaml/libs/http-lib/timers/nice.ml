(** [nice delta] changes the nice value of the current thread on Linux by [delta], and return the new value.
  [nice 0] can be used to query the current value without altering it.
 *)
external nice: int -> int = "ml_nice"
