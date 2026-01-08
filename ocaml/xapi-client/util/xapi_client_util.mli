val raise_for_task_exn :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> unit
(** [raise_for_task_exn ~rpc ~session_id task] checks the status of [task].
    @raises Api_errors.server_error if the task failed, together with a
    server-side backtrace *)

val result_of_task :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task
  -> (Rpc.t, exn * Printexc.raw_backtrace) Result.t
(** [result_of_task ~rpc ~session_id task] returns the result of [task], or a
    an exception and a backtrace.
    Consumes [task] by destroying it at the end.
*)

val results_of_tasks :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> API.ref_task list
  -> (Rpc.t, exn * Printexc.raw_backtrace) Result.t list
(** [results_of_tasks ~rpc ~session_id tasks] returns the results of [tasks], or a
    an exception and a backtrace.
    Consumes [tasks] by destroying them at the end.
    If any task fails then it cancels all other pending tasks.
*)
