open Etcd_service_types
open Lwt.Syntax

module Make (B : KVBackendLwt) = struct
  type t = unit Lwt.t * unit Lwt.u

  module StringMap = Map.Make (String)

  let json_of_body body =
    let* body = Cohttp_lwt.Body.to_string body in
    body |> Yojson.Basic.from_string |> Lwt.return

  let respond_json ~status json =
     let body = json |> Yojson.Basic.to_string ~std:true in
     (* TODO: handle exceptions *)
     let headers = Cohttp.Header.of_list ["Content-Type", "application/json"] in
     Cohttp_lwt_unix.Server.respond_string ~headers ~status ~body ()

  let service_map =
    Etcd_service.make (module B)
    |> List.fold_left
         (fun acc (Etcd_service.RPC (rpc, f)) ->
           let handler json =
             let* response = json |> rpc.decode_json |> f in
             let status, json =
               match response with
               | Ok r -> `OK, rpc.encode_json r
               | Error status ->
                   Status.http_status_of_code status.Status.code,
                   Status.to_yojson status
             in
             respond_json ~status json
           in
           (* deprecated, but old etcd versions have only this,
              new etcds don't have it at all?
               *)
           let name2 =
             "/v3alpha"
             ^ String.sub rpc.http_name 3 (String.length rpc.http_name - 3)
           in
           acc
           |> StringMap.add rpc.http_name handler
           |> StringMap.add name2 handler
         )
         StringMap.empty

  open Cohttp_lwt_unix

  let callback _conn req body =
    Logs.debug (fun m -> m "Received request: %a" Cohttp.Request.pp_hum req) ;
    match Cohttp.Request.meth req with
    | `POST -> (
        let uri = req |> Cohttp.Request.uri in
        let uri_path = uri |> Uri.path in
        match StringMap.find_opt uri_path service_map with
        | Some handler ->
            let* json = json_of_body body in
            handler json
        | None ->
            Logs.info (fun m -> m "Handler for path %s not found" uri_path) ;
            (* TODO: what is right response here? *)
            Server.respond_not_found ~uri ()
      )
    | _ ->
        Server.respond_error ~status:`Method_not_allowed
          ~body:"Method Not Allowed" ()

  let internal_error =
    let status = Status.{ code = Code_types.Internal; message = Some "Internal error" } in
    Status.http_status_of_code status.Status.code,
    Status.to_yojson status

  let callback conn req body =
    Lwt.catch (fun () -> callback conn req body)
    (fun e ->
      let bt = Printexc.get_raw_backtrace () in
      Logs.err (fun m -> m "Internal error: %a" Fmt.exn_backtrace (e, bt));
      let status, json = internal_error in
      respond_json ~status json
    )

  let listen sockaddr =
    (*
    We could do the following, but it doesn't provide a way to let us know when
    the socket got created:
    let* ctx, mode = match sockaddr with
    | Unix.ADDR_UNIX path ->
        Lwt.return (Lazy.force Conduit_lwt_unix.default_ctx, `Unix_domain_socket (`File path))
    | Unix.ADDR_INET (inet, port) ->
        let src = Unix.string_of_inet_addr inet in
        let+ ctx = Conduit_lwt_unix.init ~src () in
        ctx, `TCP (`Port port)
    in
    let ctx = Net.init ~ctx () in
    *)
    (* TODO: factor out *)
    let domain =
      match sockaddr with
      | Unix.ADDR_UNIX _ ->
          Unix.PF_UNIX
      | Unix.ADDR_INET (inet, _) ->
          if Unix.is_inet6_addr inet then Unix.PF_INET6 else Unix.PF_INET
    in
    let socket = Lwt_unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
    let+ () = Lwt_unix.bind socket sockaddr in
    Lwt_unix.listen socket 128 ;
    let mode = `TCP (`Socket socket) in
    let stop, do_stop = Lwt.wait () in
    (Server.create ~stop ~mode (Server.make ~callback ()), do_stop)

  let shutdown (finished, do_stop) = Lwt.wakeup do_stop () ; finished
end
