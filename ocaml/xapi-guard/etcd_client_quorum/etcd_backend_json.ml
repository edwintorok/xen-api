open Lwt.Syntax

type 'a io = 'a Lwt.t

(* Cohttp_lwt_unix.Connection_cache.t is part of upcoming Cohttp 6.x,
   for now no cache

  type t = { cache: Cohttp_lwt_unix.Connection_cache.t }
let init () = { cache = Cohttp_lwt_unix.Connection_cache.create () }
 *)

type t = unit


let name = __MODULE__

let init () = ()

let cleanup _ = Lwt.return_unit

let local_uri = Uri.of_string "http://localhost:2379/"

let proxy rpc_desc () request =
  let open Etcd_service in
  let body =
    request
    |> rpc_desc.encode_json_request
    |> Yojson.Basic.to_string ~std:true
  in
  let uri = Uri.with_path local_uri (rpc_desc.http_name) in
  (* Format.eprintf "%a: %s@," Uri.pp uri body; *)
  let body = Cohttp_lwt.Body.of_string body in
  let* r, body =
    Cohttp_lwt_unix.Client.call ~body `POST uri
  in
  match Cohttp.Response.status r with
  | `OK ->
      let+ body = Cohttp_lwt.Body.to_string body in
      body |> Yojson.Basic.from_string |> rpc_desc.decode_json_response
      |> Result.ok
  | http_status ->
      let+ body = Cohttp_lwt.Body.to_string body in
      match body |> Yojson.Safe.from_string |>
      Status.of_yojson with
      | Ok status -> Result.error status
      | Error e ->
          let status = Status.{code = Code_types.Unknown; message = Some ("Unknown error: " ^ e)} in
          Result.error status
      | exception Yojson.Json_error _ ->
          let msg = Printf.sprintf "%s: HTTP %d, %s" (Uri.to_string uri) (Cohttp.Code.code_of_status http_status) body in
          let status = Status.{code = Code_types.Unknown; message = Some msg } in
          Result.error status
        

let put = proxy Etcd_service.put

let range = proxy Etcd_service.range

let delete_range = proxy Etcd_service.delete_range
