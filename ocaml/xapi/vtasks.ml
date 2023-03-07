type 'a t = 'a Serializable.typ * Context.t * API.ref_task

type task = API.ref_task

type context = Context.t

let v ~__context  (typ_of: 'a Rpc.Types.typ) (f: unit -> task) : 'a t =
  typ_of, __context, f ()

let await_exn (typ_of, __context, t) =
  let of_rpc rpc =
    Rpcmarshal.unmarshal typ_of rpc
    |> function
    | Ok r -> r
    | Error (`Msg m) -> Fmt.failwith "Unmarshaling failed: %s" m
  in
  Helpers.Task.wait_for ~__context ~tasks:[t];
  Helpers.Task.to_result ~__context ~of_rpc ~t
