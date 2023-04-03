open Etcd_rpc_types
open Kv_types

let keys = Hashtbl.create 7
let revisions = ref 0

let make_response_header () =
  let revision = !revisions |> Int64.of_int in
  Some (Etcd_rpc_types.default_response_header ~revision ())

(* an in-memory backend for benchmarking *)
let put (put : put_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let prev =
    Hashtbl.find_opt keys put.key in
  incr revisions;
  let now = !revisions |> Int64.of_int in
  Hashtbl.replace keys put.key
  { key = put.key
  ; value = put.value
  ; create_revision = Option.map (fun t -> t.create_revision) prev |> Option.value ~default:now
  ; mod_revision = now
  ; version = 1L
  ; lease = 0L
  };
  Lwt.return
  { header = make_response_header ()
  ; prev_kv =
    if put.prev_kv then prev else None
  }

let range (range : range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let kvs = Hashtbl.find_all keys range.key in
  Lwt.return { header = make_response_header ()
  ; kvs
  ; more = false
  ; count = List.length kvs |> Int64.of_int
  }

let delete_range (deleterange : delete_range_request ) =
  (* TODO: check that there are no other fields set that we do not support *)
  let prev = Hashtbl.find_all keys deleterange.key
  in
  Hashtbl.remove keys deleterange.key;
  Lwt.return { header = make_response_header ()
  ; prev_kvs = if deleterange.prev_kv then prev else []
  ; deleted = List.length prev |> Int64.of_int
  }

