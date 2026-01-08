open Client

let raise_for_task_exn ~rpc ~session_id remote_task =
  match Client.Task.get_status ~rpc ~session_id ~self:remote_task with
  | `cancelling | `cancelled ->
      raise
        (Api_errors.Server_error
           (Api_errors.task_cancelled, [Ref.string_of remote_task])
        )
  | `pending ->
      failwith "wait_for_task_completion failed; task is still pending"
  | `success ->
      ()
  | `failure ->
      let error_info =
        Client.Task.get_error_info ~rpc ~session_id ~self:remote_task
      in
      let trace =
        Client.Task.get_backtrace ~rpc ~session_id ~self:remote_task
      in
      let exn =
        match error_info with
        | code :: params ->
            Api_errors.Server_error (code, params)
        | [] ->
            Failure
              (Printf.sprintf "Task failed but no error recorded: %s"
                 (Ref.string_of remote_task)
              )
      in
      Backtrace.(add exn (t_of_sexp (Sexplib.Sexp.of_string trace))) ;
      raise exn

let rpc_of_task = function "" -> Rpc.Null | s -> Xmlrpc.of_string s

let result_of_task ~rpc ~session_id self =
  match raise_for_task_exn ~rpc ~session_id self with
  | exception e ->
      Error (e, Printexc.get_raw_backtrace ())
  | () ->
      Client.Task.get_result ~rpc ~session_id ~self |> rpc_of_task |> Result.ok
