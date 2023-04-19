open Etcd_rpc_types
open Kv_types

type 'a io = 'a
type t  =
  { keys: (string, key_value) Hashtbl.t
  ; mutable revisions: int
  }

let name = __MODULE__

let init () = { keys = Hashtbl.create 7; revisions = 0 }

let cleanup t = Hashtbl.reset t.keys

let make_response_header conn =
  let revision = conn.revisions |> Int64.of_int in
  Some (Etcd_rpc_types.default_response_header ~revision ())

(* an in-memory backend for benchmarking *)
let put t (put : put_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let prev = Hashtbl.find_opt t.keys put.key in
  t.revisions <- t.revisions + 1;
  let now = t.revisions |> Int64.of_int in
  Hashtbl.replace t.keys put.key
    {
      key= put.key
    ; value= put.value
    ; create_revision=
        Option.map (fun t -> t.create_revision) prev
        |> Option.value ~default:now
    ; mod_revision= now
    ; version= 1L
    ; lease= 0L
    } ;
  Result.ok {
      header= make_response_header t
    ; prev_kv= (if put.prev_kv then prev else None)
    }

let range t (range : range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let kvs = Hashtbl.find_all t.keys range.key in
  Result.ok
    {
      header= make_response_header t
    ; kvs
    ; more= false
    ; count= List.length kvs |> Int64.of_int
    }

let delete_range t (deleterange : delete_range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let prev = Hashtbl.find_all t.keys deleterange.key in
  Hashtbl.remove t.keys deleterange.key ;
  Result.ok
    {
      header= make_response_header t
    ; prev_kvs= (if deleterange.prev_kv then prev else [])
    ; deleted= List.length prev |> Int64.of_int
    }
