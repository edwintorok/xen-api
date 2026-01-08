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

let on_task f task =
  (* Task may have been GCed meanwhile, or changed state on its own,
     ignore exceptions when canceling and destroying *)
  try f task with Api_errors.Server_error (_, _) -> ()

let result_of_task ~rpc ~session_id self =
  let call f = f ~rpc ~session_id in
  let outcome =
    try
      call @@ raise_for_task_exn self ;
      call @@ Client.Task.get_result ~self |> rpc_of_task |> Result.ok
    with e -> Error (e, Printexc.get_raw_backtrace ())
  in
  let () = self |> on_task @@ fun self -> call @@ Client.Task.destroy ~self in
  outcome

let results_of_tasks ~rpc ~session_id tasks =
  let call f = f ~rpc ~session_id in
  let callback _ self =
    let () =
      if call @@ Client.Task.get_status ~self = `failure then
        tasks
        |> List.filter (( <> ) self)
        |> List.iter @@ on_task @@ fun task -> call @@ Client.Task.cancel ~task
    in
    []
  in
  call @@ Tasks.wait_for_all_with_callback ~tasks ~callback ;
  tasks |> List.map (call @@ result_of_task)

let map_async ~rpc ~session_id f lst =
  let call f = f ~rpc ~session_id in
  lst |> List.map (call @@ f) |> call @@ results_of_tasks

let with_objects ~rpc ~session_id input create destroy f =
  let call f = f ~rpc ~session_id in
  let objects = input |> call @@ map_async create in
  (* TODO: ensure exceptions don't escape, wrap *)
  let outcomes = List.map f objects in
  let (_ : _ list) = objects |> call @@ map_async destroy in
  outcomes
