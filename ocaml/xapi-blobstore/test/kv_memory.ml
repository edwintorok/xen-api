open Xapi_blobstore_core
open Types

type 'a io = 'a

type t = (key, value) Hashtbl.t

let name = __MODULE__

let connect () = Hashtbl.create 47

let disconnect _ = ()

let get t k = Hashtbl.find_opt t k

let put t k v =
  let prev = Hashtbl.find_opt t k in
  Hashtbl.replace t k v ; prev

let delete t k =
  let prev = Hashtbl.find_opt t k in
  Hashtbl.remove t k ; prev

let list t = t |> Hashtbl.to_seq_keys |> List.of_seq
