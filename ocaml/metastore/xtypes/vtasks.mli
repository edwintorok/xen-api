(** tasks with return type 'a *)
type 'a t

type task (** type for API tasks. API.ref_task inside XAPI, something simpler in unit tests *)

type context (** the type for contexts. To avoid a circular dependency with XAPI *)

val v : __context:context -> 'a Rpc.Types.typ -> (unit -> task) -> 'a t
(** [v ~__context typ_of f] wraps a call to a task with given deserializer for its return
    value () *)

val await_exn : 'a t -> 'a
(** [await_exn task] waits for [task] to be completed. *)
