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

type t = {rpc:Rpc.call -> Rpc.response; session_id: API.ref_session}
type client = t
type outcome = (Rpc.t, exn * Printexc.raw_backtrace) result

let make ~rpc ~session_id =
  {rpc;session_id}

let call {rpc;session_id} f = f ~rpc ~session_id

module Task = struct
  type t = API.ref_task

  let allowed_operations client self =
    try call client @@ Client.Task.get_allowed_operations ~self
    with Api_errors.Server_error _ -> []

  let if_allowed client op f self =
    if self |> allowed_operations client |> List.mem op then
      (* there could still be a race condition here, so ignore API errors *)
      try f ()
      with Api_errors.Server_error _ -> ()

  let try_cancel client self =
    let perform () = call client @@ Client.Task.cancel ~task:self in
    if_allowed client `cancel perform self

  let destroy client self =
    let perform () = call client @@ Client.Task.destroy ~self in
    if_allowed client `destroy perform self

  let rpc_of_task = function "" -> Rpc.Null | s -> Xmlrpc.of_string s

  let result client self =
    try
      call client @@ raise_for_task_exn self ;
      call client @@ Client.Task.get_result ~self |> rpc_of_task |> Result.ok
    with e -> Error (e, Printexc.get_raw_backtrace ())
end

let run client ?(on_task_complete = fun _ -> []) tasks =
  let callback _ task = on_task_complete task in
  call client @@ Tasks.wait_for_all_with_callback ~tasks ~callback

(*let with_objects ~rpc ~session_id input create destroy f =
  let call f = f ~rpc ~session_id in
  let objects = input |> List.map (call @@ f) |> call @@ results_of_tasks in
  (* TODO: ensure exceptions don't escape, wrap *)
  let outcomes = List.map f objects in
  let (_ : _ list) = objects |> call @@ map_async destroy in
  outcomes
  *)
