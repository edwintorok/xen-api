open Lwt.Syntax
module P = Etcd_rpc_pb
module J = Etcd_rpc_yojson
module F = Etcd_rpc_pp

type ('a, 'b) rpc =
  { name: string
  ; http_name: string
  ; decode_grpc: Pbrt.Decoder.t -> 'a
  ; decode_json: Yojson.Basic.t -> 'a
  ; encode_grpc: 'b -> Pbrt.Encoder.t -> unit
  ; encode_json: 'b -> Yojson.Basic.t
  }

type t = RPC: ('a, 'b) rpc * ('a -> 'b Lwt.t) -> t

let make_rpc f name http_name (decode_grpc, decode_json, pp_request) (encode_grpc, encode_json, pp_response) =
  let f req =
    Logs.debug (fun m -> m "Received: %a" pp_request req);
    let+ response = f req in
    Logs.debug (fun m -> m "Reply: %a" pp_response response);
    response
  in
  RPC ({ name; http_name; decode_grpc; decode_json; encode_grpc; encode_json }, f)

let make (module B: Etcd_service_types.KVBackend) =
  (* TODO: we will need to patch ocaml-protoc to handle 'bytes' as base64 for
     JSON, by default it is not implemented! *)
  (* matches entries in etcd's rpc.proto [service KV] *)
  [ make_rpc B.range "Range" "/v3/kv/range"
    (P.decode_range_request, J.decode_range_request, F.pp_range_request)
    (P.encode_range_response, J.encode_range_response, F.pp_range_response)
  ; make_rpc B.put "Put" "/v3/kv/put"
    (P.decode_put_request, J.decode_put_request, F.pp_put_request)
    (P.encode_put_response, J.encode_put_response, F.pp_put_response)
  ; make_rpc B.delete_range "DeleteRange" "/v3/kv/deleterange"
    (P.decode_delete_range_request, J.decode_delete_range_request, F.pp_delete_range_request)
    (P.encode_delete_range_response, J.encode_delete_range_response, F.pp_delete_range_response)
  ]
