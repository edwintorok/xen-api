val raise_for_task_exn :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> unit
(** [raise_for_task_exn ~rpc ~session_id task] checks the status of [task].
    @raises Api_errors.server_error if the task failed, together with a
    server-side backtrace *)

(** a client state: [rpc] and [session_id] *)
type t

type client = t

type outcome = (Rpc.t, exn * Printexc.raw_backtrace) result

val make : rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> t
(** [make ~rpc ~session_id] constructs client state {!type:t} *)

val call :
     t
  -> (rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a)
  -> 'a

module Task : sig
  type t = API.ref_task

  val try_cancel : client -> t -> unit
  (** [try_cancel client t] signals task [t] to cancel, if it is cancelable.
      It doesn't wait for the cancel to complete *)

  val destroy : client -> t -> unit
  (** [destroy client t] destroys task [t], if it still exists. *)

  val result : client -> t -> outcome
  (** [result client t] is the current result of task [t]. *)
end

val run : t -> ?on_task_complete:(Task.t -> Task.t list) -> Task.t list -> unit
(** [run client ?on_task_complete tasks] waits until a task from [tasks] is no
  longer pending and invokes [on_task_complete] for each such task.
  This callback is allowed to modify the state of other tasks (e.g. cancel them),
  and can return more tasks to be run instead of the current one.
  When all tasks are no longer pending then [run] returns.
*)
