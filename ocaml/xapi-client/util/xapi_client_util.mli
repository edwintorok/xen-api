val raise_for_task_exn :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> unit
(** [raise_for_task_exn ~rpc ~session_id task] checks the status of [task].
    @raises Api_errors.server_error if the task failed, together with a
    server-side backtrace *)

val result_of_task:
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> (Rpc.t, exn * Printexc.raw_backtrace) Result.t
