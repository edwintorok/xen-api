open Client.Client

module D = Debug.Make (struct let name = "quicktest_memory_properties" end)

let call (rpc, session_id) f = f ~rpc ~session_id

let result_of_task ctx self =
  try
    match call ctx @@ Task.get_status ~self with
    | `pending ->
        None
    | `success ->
        call ctx @@ Task.get_result ~self |> Result.ok |> Option.some
    | `cancelling ->
        Failure "Cancelling" |> Result.error |> Option.some
    | `cancelled ->
        Failure "Cancelled" |> Result.error |> Option.some
    | `failure -> (
      match call ctx @@ Task.get_error_info ~self with
      | [] ->
          Failure "Unknown error" |> Result.error |> Option.some
      | code :: params ->
          Api_errors.Server_error (code, params) |> Result.error |> Option.some
    )
  with Api_errors.Server_error (_, _) as e -> e |> Result.error |> Option.some

let cancel_all ctx tasks =
  tasks
  |> List.iter @@ fun task ->
     if Option.is_none (result_of_task ctx task) then (
       (* This is inherently racy, the task could've finished or failed on its
          own meanwhile *)
       D.log_and_ignore_exn
       @@ fun () ->
       D.debug "cancel_all: Canceling task %s" Ref.(string_of task) ;
       call ctx @@ Task.cancel ~task
     )

let wait_for_all_or_cancel ctx tasks =
  let callback _ task =
    match result_of_task ctx task with
    | Some (Ok _) | None ->
        []
    | Some (Error _) ->
        tasks |> List.filter (( <> ) task) |> cancel_all ctx ;
        (* the wait_for_all loop will then see the tasks getting cancelled and
           exit, no need to raise exceptions here *)
        []
  in
  call ctx @@ Tasks.wait_for_all_with_callback ~tasks ~callback

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
