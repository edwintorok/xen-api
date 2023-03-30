open Lwt.Syntax
open Etcd_rpc_types
open Kv_types
open Etcd_rpc_yojson

let json_of_body body =
  let* body = Cohttp_lwt.Body.to_string body in
  body |> Yojson.Basic.from_string
  |> Lwt.return

let revisions = ref 0
let make_response_header () =
  let revision = !revisions |> Int64.of_int in
  Some { cluster_id = 1L
  ; member_id = 1L
  ; revision
  ; raft_term = 1L;
  }

let keys = Hashtbl.create 7

let v3_kv_put (put : put_request) =
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
  Lwt.return { header = make_response_header ()
  ; prev_kv =
    if put.prev_kv then prev else None
  }

let v3_kv_range (range : range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let kvs = Hashtbl.find_all keys range.key in
  Lwt.return { header = make_response_header ()
  ; kvs
  ; more = false
  ; count = List.length kvs |> Int64.of_int
  }

let v3_kv_deleterange (deleterange : delete_range_request ) =
  (* TODO: check that there are no other fields set that we do not support *)
  let prev = Hashtbl.find_all keys deleterange.key
  in
  Hashtbl.remove keys deleterange.key;
  Lwt.return { header = make_response_header ()
  ; prev_kvs = if deleterange.prev_kv then prev else []
  ; deleted = List.length prev |> Int64.of_int
  }

let listen mode =
  let open Cohttp_lwt_unix in

  let callback _conn req body =
    Logs.debug (fun m -> m "got request: %a" Cohttp.Request.pp_hum req);
    match Request.meth req with
    | `POST ->
      let* json = json_of_body body in
      let do_rpc decode encode f =
        let* response = json |> decode |> f in
        let body = encode response |> Yojson.Basic.to_string ~std:true in
        Logs.debug (fun m -> m "responding with: %s" body);
        Server.respond_string ~status:`OK ~body ()
      in
      (* @see https://etcd.io/docs/v3.5/dev-guide/api_grpc_gateway/#put-and-get-keys *)
      (match req |> Request.uri |> Uri.path with
      | "/v3/kv/put" ->
          do_rpc decode_put_request encode_put_response v3_kv_put
      | "/v3/kv/range" ->
          do_rpc decode_range_request encode_range_response v3_kv_range
      | "/v3/kv/deleterange" ->
          do_rpc decode_delete_range_request encode_delete_range_response v3_kv_deleterange
      | _ ->
          (* TODO: what is right response here? *)
          Server.respond_not_found ()
      )
    | _ ->
      Server.respond_error ~status:`Method_not_allowed ~body:"Method_not_allowed" ()
  in
  Server.create ~mode (Server.make ~callback ())

let mode_of_fd fd : Conduit_lwt_unix.server =
  `TCP (`Socket (Lwt_unix.of_unix_file_descr fd))

let server =
  let fds = Daemon.listen_fds () in
  let modes = fds |> List.map mode_of_fd in
  let mode = `TCP (`Port 12380) in
  let* (_: _ list) =
    (mode :: modes) |>
    Lwt_list.map_p listen
  in
  Lwt.return_unit

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Logs.set_level ~all:true (Some Logs.Debug);
  (* default would be 500% overhead, 120% here matches the value used for
     [space_overhead], and is what oxenstored uses. *)
  Gc.set { (Gc.get ()) with Gc.max_overhead = 120 };

  (* the default would be 1000 threads, set a more reasonable limit *)
  Lwt_unix.set_pool_size 16;

  Lwt_main.run server
