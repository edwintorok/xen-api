open Client.Client

module D = Debug.Make (struct let name = "quicktest_memory_properties" end)

let call (rpc, session_id) f = f ~rpc ~session_id

let result_of_task ~rpc ~session_id self =
  let call f = f ~rpc ~session_id in
  try
    match call @@ Task.get_status ~self with
    | `pending ->
        (* Task cannot still be pending, we've already waited for it in Tasks.wait_for_all *)
        assert false
    | `success ->
        call @@ Task.get_result ~self |> Result.ok
    | `cancelling | `cancelled ->
        Result.error [Api_errors.task_cancelled; Ref.string_of self]
    | `failure ->
        call @@ Task.get_error_info ~self |> Result.error
  with Api_errors.Server_error (code, params) -> Result.error (code :: params)

let for_all_tasks f tasks =
  tasks
  |> List.iter @@ fun task ->
     (* Task may have been GCed meanwhile, or changed state on its own *)
     D.log_and_ignore_exn @@ fun () -> f task

let cancel_all ~rpc ~session_id tasks =
  let call f = f ~rpc ~session_id in
  tasks
  |> for_all_tasks @@ fun task ->
     if call @@ Task.get_status ~self:task = `pending then
       call @@ Task.cancel ~task

let wait_for_all_or_cancel ~rpc ~session_id tasks =
  let call f = f ~rpc ~session_id in
  let callback _ task =
    let () =
      task
      |> call @@ result_of_task
      |> Result.iter_error @@ fun _ ->
         tasks |> List.filter (( <> ) task) |> call @@ cancel_all
    in
    []
  in
  call @@ Tasks.wait_for_all_with_callback ~tasks ~callback ;
  (* Tasks may have been GC-ed already *)
  tasks
  |> List.iter @@ fun self ->
     D.log_and_ignore_exn @@ fun () -> call @@ Task.destroy ~self

let wait_for_results ctx tasks =
  wait_for_all_or_cancel ctx tasks ;
  (* wait_for_all ensures that we have no pending tasks, thus the Option.get
     below should be safe *)
  List.map (result_of_task ctx) tasks |> List.map Option.get

let either_of_result = function
  | Ok ok ->
      Either.Left ok
  | Error err ->
      Either.Right err

let wait_for_results_exn ctx tasks =
  let results, errors =
    tasks |> wait_for_results ctx |> List.partition_map either_of_result
  in
  let failures, others =
    List.partition (function Failure _ -> true | _ -> false) errors
  in
  match (others, failures) with
  | err :: _, _ ->
      raise err
      (* raise an exception other than
         Cancelling/Cancelled if any *)
  | _, err :: _ ->
      raise err
  (* if all we've got are cancelled tasks, then
     raise that *)
  | [], [] ->
      results
