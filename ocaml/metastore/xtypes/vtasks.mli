(** tasks with return type 'a *)
type 'a t

(** type for API tasks. API.ref_task inside XAPI, something simpler in unit tests *)
type task

(** the type for contexts. To avoid a circular dependency with XAPI *)
type context

val v : __context:context -> 'a Rpc.Types.typ -> (unit -> task) -> 'a t
(** [v ~__context typ_of f] wraps a call to a task with given deserializer for its return
    value () *)

val await_exn : 'a t -> 'a
(** [await_exn task] waits for [task] to be completed. *)
