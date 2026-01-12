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

type +'a outcome = ('a, exn * Printexc.raw_backtrace) result

type client = {rpc: Rpc.call -> Rpc.response; session_id: API.ref_session}

type +'a api =
  rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> 'a

let call {rpc; session_id} f = f ~rpc ~session_id

module AsyncTask = struct
  type +'a t = {task: API.ref_task; t_of_rpc: Rpc.t -> 'a}

  let v t_of_rpc task = {task; t_of_rpc}

  let compare a b = Ref.compare a.task b.task

  let allowed_operations t self =
    try call t @@ Client.Task.get_allowed_operations ~self:self.task
    with Api_errors.Server_error _ -> []

  let if_allowed t op f self =
    let allowed = allowed_operations t self in
    if List.mem op allowed then
      (* there could still be a race condition here, so ignore API errors *)
      try call t f with Api_errors.Server_error _ -> ()

  let try_cancel t self =
    if_allowed t `cancel Client.Task.(cancel ~task:self.task) self

  let destroy t self =
    if_allowed t `destroy Client.Task.(destroy ~self:self.task) self

  let rpc_of_task = function "" -> Rpc.Null | s -> Xmlrpc.of_string s

  let result_exn t self =
    call t @@ raise_for_task_exn self.task ;
    call t @@ Client.Task.get_result ~self:self.task
    |> rpc_of_task
    |> self.t_of_rpc

  let result t self =
    try result_exn t self |> Result.ok
    with Api_errors.Server_error _ as e ->
      Error (e, Printexc.get_raw_backtrace ())

  let task t = t.task
end

let run t ?(on_task_complete = fun _ _ _ -> []) ?(on_progress = fun _ _ -> ())
    tasks =
  let callback completed task = on_task_complete t completed task in
  let finally () = tasks |> List.iter AsyncTaskcTask.(destroy t) in
  let tbl = Hashtbl.create 7 in
  let overall = ref 0. in
  let count = List.length tasks |> float_of_int in
  let on_progress task completed progress =
    let old = Hashtbl.find_opt tbl task |> Option.value ~default:0. in
    overall := !overall -. old +. progress ;
    Hashtbl.replace tbl task progress ;
    on_progress completed (!overall /. count)
  in
  Fun.protect ~finally @@ fun () ->
  call t
  @@ Tasks.wait_for_all_with_progress ~tasks:(List.map Task.task tasks)
       ~callback ~on_progress ;
  tasks |> List.map (Task.result t)

module P = Cli_progress_bar.Make (struct
  type t = float

  let to_float = Fun.id
end)

let run_or_cancel t ?on_progress tasks =
  let p = P.create 80 0. 1. in
  let total = List.length tasks in
  let on_progress_cli completed progress =
    if P.update p progress then
      Printf.eprintf "\r%s%!" P.(string_of_bar p) ;
    if completed = total then
      Printf.eprintf "\n%s%!" P.(summarise p)
  in
  let on_progress = Option.value ~default:on_progress_cli on_progress in
  let on_task_complete t _ task =
    let () =
      if call t @@ Client.Task.get_status ~self:task = `failure then
        tasks
        |> List.filter (fun t -> Task.task t <> task)
        |> List.iter AsyncTaskcTask.(try_cancel t)
    in
    []
  in
  run t ~on_task_complete ~on_progress tasks

let task client t_of_rpc f = call client @@ f |> Task.v t_of_rpc

let calls t f args =
  let tasks = List.map (f t) args in
  run_or_cancel t tasks

let either_of_result = function
  | Ok ok ->
      Either.Left ok
  | Error err ->
      Right err

let either_of_exn = function
  | (Api_errors.Server_error _, _) as e ->
      Either.Left e
  | e ->
      Right e

let with_objects_exn t create destroy f inputs =
  let objects = calls t create inputs in
  let finally () =
    let (_ : _ list) =
      objects |> List.filter_map Result.to_option |> calls t destroy
    in
    ()
  in
  Fun.protect ~finally @@ fun () ->
  let objects, errors = List.partition_map either_of_result objects in
  (* avoid raising the canceled task exception when possible *)
  match List.partition_map either_of_exn errors with
  | (exn, bt) :: _, _ | [], (exn, bt) :: _ ->
      Printexc.raise_with_backtrace exn bt
  | [], [] ->
      f t objects
