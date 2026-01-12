val raise_for_task_exn :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> unit
(** [raise_for_task_exn ~rpc ~session_id task] checks the status of [task].
    @raises Api_errors.server_error if the task failed, together with a
    server-side backtrace *)

type +'a outcome = ('a, exn * Printexc.raw_backtrace) result

type client = {rpc: Rpc.call -> Rpc.response; session_id: API.ref_session}

type +'a api =
  rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a

val call : client -> 'a api -> 'a

module AsyncTask : sig
  type +'a t

  val v : (Rpc.t -> 'a) -> API.ref_task -> 'a t

  val compare : 'a t -> 'a t -> int

  val try_cancel : client -> _ t -> unit
  (** [try_cancel client t] signals task [t] to cancel, if it is cancelable.
      It doesn't wait for the cancel to complete *)

  val destroy : client -> _ t -> unit
  (** [destroy client t] destroys task [t], if it still exists. *)

  val result : client -> 'a t -> 'a outcome
  (** [result client t] is the current result of task [t]. *)
end

val task : client -> (Rpc.t -> 'a) -> API.ref_task api -> 'a AsyncTask.t

val run_or_cancel :
     client
  -> ?on_progress:(int -> float -> unit)
  -> 'a AsyncTask.t list
  -> 'a outcome list

val calls : client -> (client -> 'a -> 'b AsyncTask.t) -> 'a list -> 'b outcome list

val with_objects_exn :
     client
  -> (client -> 'a -> 'b AsyncTask.t)
  -> (client -> 'b -> unit AsyncTask.t)
  -> (client -> 'b list -> 'c)
  -> 'a list
  -> 'c
