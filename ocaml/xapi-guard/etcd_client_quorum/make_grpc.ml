open Etcd_service_types
open Lwt.Syntax

module Make(B: KVBackend) = struct
  type t = Lwt_io.server

  let init = Grpc_lwt.Server.Service.v ()

  let service =
      (* Lwt does co-operative multi-threading, so we can reuse the encoder,
         as long as during encoding we don't switch to another Lwt thread.
         Switching Lwt threads is well defined, happens only in [Lwt.bind].
       *)
      let encoder = Pbrt.Encoder.create () in
      Etcd_service.make (module B)
      |> List.fold_left (fun acc (Etcd_service.RPC (rpc, f)) ->
          let unary buffer =
            Pbrt.Encoder.clear encoder;
            let+ reply = buffer |> Pbrt.Decoder.of_string |> rpc.decode_grpc |> f in
            rpc.encode_grpc reply encoder;
            (* TODO: handle exceptions *)
            Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder)
          in
          Grpc_lwt.Server.Service.add_rpc ~name:rpc.name ~rpc:(Unary unary) acc
      ) init
      |> Grpc_lwt.Server.Service.handle_request

  let grpc_server =
    Grpc_lwt.Server.(v () |> add_service ~name:"etcdserverpb.KV" ~service)

  let config = H2.Config.{
    default with enable_server_push = false
  ; max_concurrent_streams = 16l
  }

  let request_handler _ reqd =
    Logs.debug (fun m ->
      let r = H2.Reqd.request reqd in
      m "received request: %a" H2.Request.pp_hum r);
    Grpc_lwt.Server.handle_request grpc_server reqd

  let pp_conn_error ppf = function
    | `Bad_request -> Fmt.string ppf "Bad request"
    | `Internal_server_error -> Fmt.string ppf "Internal server error"
    | `Exn exn -> Fmt.pf ppf "Exception: %a" Fmt.exn exn

  let error_handler _ ?request error start_response =
    let open H2 in
    Logs.warn (fun m ->
      m "error while handling request %a: %a" Fmt.(option Request.pp_hum) request pp_conn_error error
    );
    let response_body = start_response Headers.empty in
    Body.Writer.write_string response_body
      "There was an error handling your request";
    Body.Writer.close response_body

  let listen sockaddr =
    let h2_server =
      H2_lwt_unix.Server.create_connection_handler ~config
      ~request_handler
      ~error_handler
    in
    Lwt_io.establish_server_with_client_socket sockaddr h2_server

  let shutdown = Lwt_io.shutdown_server
end
