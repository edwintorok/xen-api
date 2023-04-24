open Xapi_blobstore_core

module Key = BoundedString.Make (struct let max_length = 1024 end)

module Value = BoundedString.Make (struct let max_length = 128 * 1024 end)

type config = unit
let max_keys = 128
let max_data = 256 * 1024

type 'a io = 'a

type t = {m: Mutex.t; tbl: (Key.t, Value.t) Hashtbl.t; mutable size: int}

let with_mutex t f =
  Mutex.lock t.m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.m) (fun () -> f t.tbl)

let name = __MODULE__

let connect () = {m= Mutex.create (); tbl= Hashtbl.create 47; size= 0}

let disconnect _ = ()

let get t k = with_mutex t @@ fun tbl -> Hashtbl.find_opt tbl k

let kv_length k v =
  String.length (Key.to_string k) + String.length (Value.to_string v)

let put t k v =
  with_mutex t @@ fun tbl ->
  let delta = kv_length k v in
  let next = t.size + delta in
  if next > max_data then
    Fmt.invalid_arg "max_data exceeded: %d + %d > %d" t.size delta max_data ;
  Hashtbl.replace tbl k v ;
  t.size <- next

let delete t k =
  with_mutex t @@ fun tbl ->
  let old = Hashtbl.find_opt tbl k in
  Hashtbl.remove tbl k ;
  let old_size = old |> Option.fold ~none:0 ~some:(kv_length k) in
  t.size <- t.size - old_size

let list t =
  with_mutex t @@ fun tbl -> tbl |> Hashtbl.to_seq_keys |> List.of_seq
