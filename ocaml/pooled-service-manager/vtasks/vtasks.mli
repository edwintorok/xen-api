type 'a t (** tasks with return type 'a *)

val v : (unit -> 'a) -> 'a t
(** [v f] creates a task that runs [f] and returns value of type ['a].

    This is mostly meant for use in unit tests. Real tasks should be
    constructed using [Client.Async.<...>]
 *)

val await_exn: 'a t -> 'a
(** [await_exn task] waits for [task] to be completed. *)
