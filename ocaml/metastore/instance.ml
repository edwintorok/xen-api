type ('a, 'b) t = {config: 'a; typ_of_config: 'a Serializable.typ; id: 'b Id.t}

let v typ_of_config id config = {config; typ_of_config; id}

let compare a b = Id.compare a.id b.id

let id t = t.id

let config t = t.config

let config_serializable t = Serializable.T.v t.typ_of_config t.config

let dump ppf t =
  let open Fmt.Dump in
  record
    [
      field "id" id Id.dump
    ; field "config" config_serializable Serializable.T.dump
    ]
    ppf t

let to_map t = Id.Map.singleton (t |> id |> Id.key) t

let await task = Result.bind task @@ Rresult.R.trap_exn Vtasks.await_exn

let map idmap f =
  let execute _ input =
    (* launching a task might raise an exception too,
       however we'll have to treat a failure here the same as if it failed
       remotely
    *)
    Rresult.R.trap_exn f input
  in
  (* launch all tasks first *)
  idmap
  |> Id.Map.mapi execute
  (* query results only once all tasks have been launched *)
  |> Id.Map.map await
