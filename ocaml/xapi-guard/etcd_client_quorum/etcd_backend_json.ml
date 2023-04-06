open Kv_types
open Lwt.Infix
open Lwt.Syntax

let cache = Cohttp_lwt_unix.Connection_cache.create ()


let local_uri = Uri.of_string "http://localhost:2379"

let proxy rpc_desc request =
  let open Etcd_service in
  let body = request |> rpc_desc.encode_json_request |> Yojson.Basic.to_string ~std:true |> Cohttp_lwt.Body.of_string in
  let* (r, body) = Cohttp_lwt_unix.Connection_cache.call cache ~body `POST local_uri in
  match Cohttp.Response.status r with
  | `OK ->
     let+ body = Cohttp_lwt.Body.to_string body in
     body |> Yojson.Basic.from_string |> rpc_desc.decode_json_response
  | _ ->
    Lwt.fail_with "TODO: handle error"

let put  =
  proxy Etcd_service.put

let range =
  proxy Etcd_service.range

let delete_range =
  proxy Etcd_service.delete_range
