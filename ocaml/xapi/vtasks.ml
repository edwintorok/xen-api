type 'a t = (Rpc.t -> 'a) * Context.t * API.ref_task

let v (typ_of: 'a Rpc.Types.typ) f : 'a t =
  let __context = Context.make __FUNCTION__ in
  let task = Context.get_task_id __context in
  let execute =
    Rresult.R.trap_exn @@ fun () ->
    f () |> Rpcmarshal.marshal typ_of
  in
  let ok rpc = TaskHelper.set_result ~__context (Some rpc) in
  let error (`Exn_trap (e, bt)) =
    (* copy backtrace into xapi-backtrace, TODO: xapi-backtrace should use
       raw_backtrace instead *)
    let () =
      try
        Printexc.raise_with_backtrace e bt
      with e ->
        Backtrace.is_important e;
    in
    TaskHelper.failed ~__context e in
  execute () |> Result.fold ~ok ~error;
  let of_rpc rpc =
    Rpcmarshal.unmarshal typ_of rpc
    |> function
    | Ok r -> r
    | Error (`Msg m) -> Fmt.failwith "Unmarshaling failed: %s" m
  in
  of_rpc, __context, task

let await_exn (of_rpc, __context, t) =
  Helpers.Task.wait_for ~__context ~tasks:[t];
  Helpers.Task.to_result ~__context ~of_rpc ~t
