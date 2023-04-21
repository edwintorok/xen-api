open Xapi_blobstore_core
open Types

type 'a io = 'a

type t = {m: Mutex.t; tbl: (key, value) Hashtbl.t}

let with_mutex t f =
  Mutex.lock t.m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.m) (fun () -> f t.tbl)

let name = __MODULE__

let connect () = {m= Mutex.create (); tbl= Hashtbl.create 47}

let disconnect _ = ()

let get t k = with_mutex t @@ fun tbl -> Hashtbl.find_opt tbl k

let put t k v = with_mutex t @@ fun tbl -> Hashtbl.replace tbl k v

let delete t k = with_mutex t @@ fun tbl -> Hashtbl.remove tbl k

let list t =
  with_mutex t @@ fun tbl -> tbl |> Hashtbl.to_seq_keys |> List.of_seq
