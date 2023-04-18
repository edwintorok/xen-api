open Lwt.Syntax

type t = { cache: Cohttp_lwt_unix.Connection_cache.t }

let name = __MODULE__

let init () = { cache = Cohttp_lwt_unix.Connection_cache.create () }

let cleanup _ =
  (* there is no cleanup function for the connection cache *)
  Lwt.return_unit

let local_uri = Uri.of_string "http://localhost:2379"

let proxy rpc_desc t request =
  let open Etcd_service in
  let body =
    request
    |> rpc_desc.encode_json_request
    |> Yojson.Basic.to_string ~std:true
    |> Cohttp_lwt.Body.of_string
  in
  let* r, body =
    Cohttp_lwt_unix.Connection_cache.call t.cache ~body `POST local_uri
  in
  match Cohttp.Response.status r with
  | `OK ->
      let+ body = Cohttp_lwt.Body.to_string body in
      body |> Yojson.Basic.from_string |> rpc_desc.decode_json_response
      |> Result.ok
  | _ ->
      let+ body = Cohttp_lwt.Body.to_string body in
      match body |> Yojson.Safe.from_string |>
      Status.of_yojson with
      | Ok status -> Result.error status
      | Error e ->
          let status = Status.{code = Code_types.Unknown; message = Some ("Unknown error: " ^ e)} in
          Result.error status

let put = proxy Etcd_service.put

let range = proxy Etcd_service.range

let delete_range = proxy Etcd_service.delete_range
