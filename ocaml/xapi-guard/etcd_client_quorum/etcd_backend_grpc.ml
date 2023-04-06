open Lwt.Syntax

let connect sockaddr =
  let domain =
    match sockaddr with
    | Unix.ADDR_UNIX _ ->
        Unix.PF_UNIX
    | Unix.ADDR_INET (inet, _) ->
        if Unix.is_inet6_addr inet then Unix.PF_INET6 else Unix.PF_INET
  in
  let socket = Lwt_unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket sockaddr in
  (* TODO: errhandler *)
  H2_lwt_unix.Client.create_connection ~error_handler:ignore socket

(* pool of 1 - a persistent connection *)
let pool sockaddr =
  let validate conn =
    conn |> H2_lwt_unix.Client.is_closed |> not |> Lwt.return
  in
  let check conn f = conn |> H2_lwt_unix.Client.is_closed |> not |> f in
  Lwt_pool.create 1 ~validate ~check ~dispose:H2_lwt_unix.Client.shutdown
  @@ fun () -> connect sockaddr

let persistent_conn = pool (Unix.ADDR_INET (Unix.inet_addr_loopback, 2379))

let service = "etcdserverpb.KV"

let of_grpc_status gstatus =
  let code = gstatus |> Grpc.Status.code |> Grpc.Status.int_of_code |> Status.code_of_int in
  Status.{code; message = Grpc.Status.message gstatus}

let proxy rpc_desc request =
  let open Etcd_service in
  let f response_opt_lwt =
    let* (response_opt : string option) = response_opt_lwt in
    match response_opt with
    | None ->
        Lwt.fail_with "no response?"
    | Some r ->
        r
        |> Pbrt.Decoder.of_string
        |> rpc_desc.decode_grpc_response
        |> Lwt.return_ok
  in
  (* TODO: reuse encoder *)
  let encoder = Pbrt.Encoder.create () in
  Lwt_pool.use persistent_conn @@ fun conn ->
  rpc_desc.encode_grpc_request request encoder ;
  let handler =
    encoder |> Pbrt.Encoder.to_string |> Grpc_lwt.Client.Rpc.unary ~f
  in
  let* r =
    Grpc_lwt.Client.call ~service ~rpc:rpc_desc.name ~scheme:"http" ~handler
      ~do_request:(H2_lwt_unix.Client.request conn ~error_handler:ignore)
      ()
  in
  match r with
  | Ok (Ok ret, _status) ->
      Lwt_result.return ret
  | Ok (Error err, _status) ->
      Lwt_result.fail @@ of_grpc_status err
  | Error status ->
      Lwt_result.fail @@ of_grpc_status status

let put = proxy Etcd_service.put

let range = proxy Etcd_service.range

let delete_range = proxy Etcd_service.delete_range
