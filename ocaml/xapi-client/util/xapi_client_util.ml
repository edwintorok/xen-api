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

type t = {rpc: Rpc.call -> Rpc.response; session_id: API.ref_session}

type client = t

type outcome = (Rpc.t, exn * Printexc.raw_backtrace) result

let make ~rpc ~session_id = {rpc; session_id}

let call {rpc; session_id} f = f ~rpc ~session_id

module Task = struct
  type t = API.ref_task

  let compare = Ref.compare

  let allowed_operations client self =
    try call client @@ Client.Task.get_allowed_operations ~self
    with Api_errors.Server_error _ -> []

  let if_allowed client op f self =
    if self |> allowed_operations client |> List.mem op then
      (* there could still be a race condition here, so ignore API errors *)
      try f () with Api_errors.Server_error _ -> ()

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

module TaskMap = Map.Make (Task)
module TaskSet = Set.Make (Task)

(*let with_objects ~rpc ~session_id input create destroy f =
  let call f = f ~rpc ~session_id in
  let objects = input |> List.map (call @@ f) |> call @@ results_of_tasks in
  (* TODO: ensure exceptions don't escape, wrap *)
  let outcomes = List.map f objects in
  let (_ : _ list) = objects |> call @@ map_async destroy in
  outcomes
*)
module Pending = struct
  type t = (outcome -> unit) TaskMap.t

  let set t task outcome =
    match TaskMap.find_opt task t with None -> () | Some f -> f outcome

  let action_union _ f1 f2 = Some (fun outcome -> f1 outcome ; f2 outcome)

  let union t1 t2 = TaskMap.union action_union t1 t2

  let make task =
    let result = ref None in
    let set outcome = result := Some outcome and get () = Option.get !result in
    (TaskMap.singleton task set, get)

  let run t =
end
(* TODO: not right because we don't immediately run the continuations,
   just do the task parallelization helpers, this is too complex..
 *)

module M = struct
  type +'a t =
    | Done : 'a -> 'a t
    | Pending : Pending.t * (unit -> 'b) * ('b -> 'a t) -> 'a t

  let return x = Done x

  let rec ( let+ ) t f =
    match t with
    | Done x ->
        x |> f |> return
    | Pending (input, read, continue) ->
        let next outcome =
          let+ x = continue outcome in
          f x
        in
        Pending (input, read, next)

  let rec ( let* ) t f =
    match t with
    | Done x ->
        f x
    | Pending (input, read, continue) ->
        let next outcome =
          let* x = continue outcome in
          f x
        in
        Pending (input, read, next)

  let rec ( and+ ) a b =
    match (a, b) with
    | Done x, Done y ->
        return (x, y)
    | Done x, Pending (input, read, continue) ->
        let next outcome =
          let+ y = continue outcome in
          (x, y)
        in
        Pending (input, read, next)
    | Pending (input, read, continue), Done y ->
        let next outcome =
          let+ x = continue outcome in
          (x, y)
        in
        Pending (input, read, next)
    | Pending (input1, read1, continue1), Pending (input2, read2, continue2) ->
        let next (a, b) =
          let+ x = continue1 a and+ y = continue2 b in
          (x, y)
        and read () = (read1 (), read2 ()) in
        Pending (Pending.union input1 input2, read, next)

  let ( and* ) = ( and+ )

  let rec await = function
    | Done x -> x
    | Pending  (input, read, next) ->
        Pending.run input

end
