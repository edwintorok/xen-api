open Lwt.Syntax
module P = Etcd_rpc_pb
module J = Etcd_rpc_yojson
module F = Etcd_rpc_pp

type ('a, 'b) rpc = {
    name: string
  ; http_name: string
  ; decode_grpc: Pbrt.Decoder.t -> 'a
  ; decode_grpc_response: Pbrt.Decoder.t -> 'b
  ; decode_json: Yojson.Basic.t -> 'a
  ; decode_json_response: Yojson.Basic.t -> 'b
  ; pp_request: 'a Fmt.t
  ; encode_grpc: 'b -> Pbrt.Encoder.t -> unit
  ; encode_grpc_request: 'a -> Pbrt.Encoder.t -> unit
  ; encode_json: 'b -> Yojson.Basic.t
  ; encode_json_request: 'a -> Yojson.Basic.t
  ; pp_response: 'b Fmt.t
}

let make name http_name
    ( decode_grpc
    , decode_json
    , pp_request
    , encode_grpc_request
    , encode_json_request
    )
    ( encode_grpc
    , encode_json
    , pp_response
    , decode_grpc_response
    , decode_json_response
    ) =
  {
    name
  ; http_name
  ; decode_grpc
  ; decode_json
  ; decode_json_response
  ; decode_grpc_response
  ; pp_request
  ; encode_grpc
  ; encode_json
  ; encode_grpc_request
  ; encode_json_request
  ; pp_response
  }

let range =
  make "Range" "/v3/kv/range"
    ( P.decode_range_request
    , J.decode_range_request
    , F.pp_range_request
    , P.encode_range_request
    , J.encode_range_request
    )
    ( P.encode_range_response
    , J.encode_range_response
    , F.pp_range_response
    , P.decode_range_response
    , J.decode_range_response
    )

let put =
  make "Put" "/v3/kv/put"
    ( P.decode_put_request
    , J.decode_put_request
    , F.pp_put_request
    , P.encode_put_request
    , J.encode_put_request
    )
    ( P.encode_put_response
    , J.encode_put_response
    , F.pp_put_response
    , P.decode_put_response
    , J.decode_put_response
    )

let delete_range =
  make "DeleteRange" "/v3/kv/deleterange"
    ( P.decode_delete_range_request
    , J.decode_delete_range_request
    , F.pp_delete_range_request
    , P.encode_delete_range_request
    , J.encode_delete_range_request
    )
    ( P.encode_delete_range_response
    , J.encode_delete_range_response
    , F.pp_delete_range_response
    , P.decode_delete_range_response
    , J.decode_delete_range_response
    )

type t =
  | RPC :
      ('a, 'b) rpc * ('a -> ('b, Etcd_service_types.status) Lwt_result.t)
      -> t

let make_rpc f rpc =
  let f req =
    Logs.debug (fun m -> m "Received: %a" rpc.pp_request req) ;
    let+ res = f req in
    let pp_res = Fmt.result ~ok:rpc.pp_response ~error:Status.pp in
    Logs.debug (fun m -> m "Reply: %a" pp_res res) ;
    res
  in
  RPC (rpc, f)

let make (module B : Etcd_service_types.KVBackendLwt) =
  (* TODO: we will need to patch ocaml-protoc to handle 'bytes' as base64 for
     JSON, by default it is not implemented! *)
  (* matches entries in etcd's rpc.proto [service KV] *)
  let conn = B.init () in
  (* Lwt_gc.finalise B.cleanup conn; *)
  [
    make_rpc (B.range conn) range
  ; make_rpc (B.put conn) put
  ; make_rpc (B.delete_range conn) delete_range
  ]
