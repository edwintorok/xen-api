(** tasks with return type 'a *)
type 'a t

val v : 'a Rpc.Types.typ -> (unit -> 'a) -> 'a t
(** [v typ_of f] creates a task that runs [f] and returns value of type ['a].

    This is mostly meant for use in unit tests. Real tasks should be
    constructed using [Client.Async.<...>]
 *)

val await_exn : 'a t -> 'a
(** [await_exn task] waits for [task] to be completed. *)
