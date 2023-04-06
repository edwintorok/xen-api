let code_of_int n =
  let encoder = Pbrt.Encoder.create () in
  Pbrt.Encoder.int_as_varint n encoder ;
  let decoder = encoder |> Pbrt.Encoder.to_string |> Pbrt.Decoder.of_string in
  try Code_pb.decode_code decoder
  with _ ->
    Logs.info (fun m -> m "Unkown gRPC error code %d, mapped to Unknown" n) ;
    Code_types.Unknown

let int_of_code code =
  let encoder = Pbrt.Encoder.create () in
  Code_pb.encode_code code encoder;
  let decoder = encoder |> Pbrt.Encoder.to_string |> Pbrt.Decoder.of_string in
  Pbrt.Decoder.int_as_varint decoder

type t =
  { code: Code_types.code
  ; message: string option
  }

module JSON = struct
  type t =
    { code: int
    ; error: string option
    ; message: string option
    } [@@deriving yojson {strict= false}]
end

let to_json (t: t) = JSON.{code = int_of_code t.code; message = t.message; error = t.message }
let of_json (json: JSON.t) = {code = code_of_int json.code; message = json.message }

let to_yojson t = t |> to_json |> JSON.to_yojson |> Yojson.Safe.to_basic
let of_yojson json =
  let open Ppx_deriving_yojson_runtime in
  match json |> JSON.of_yojson with
  | Result.Ok j -> Ok (of_json j)
  | Result.Error e -> Error e

let pp =
  Fmt.Dump.(
    record
      [
        field "code" (fun t -> t.code) Code_pp.pp_code
      ; field "message" (fun t -> t.message) Fmt.(option string)
      ]
  )

(** [http_status_of_code code] maps an gRPC error code to an http status
   @see https://github.com/googleapis/googleapis/blob/master/google/rpc/code.proto *)
let http_status_of_code =
  let open Code_types in
  function
  | Ok  ->
      `OK
  | Cancelled ->
      `Client_closed_request
  | Unknown ->
      `Internal_server_error
  | Invalid_argument ->
      `Bad_request
  | Deadline_exceeded ->
      `Gateway_timeout
  | Not_found ->
      `Not_found
  | Already_exists ->
      `Conflict
  | Permission_denied ->
      `Forbidden
  | Resource_exhausted ->
      `Too_many_requests
  | Failed_precondition ->
      `Bad_request
  | Aborted ->
      `Conflict
  | Out_of_range ->
      `Bad_request
  | Unimplemented ->
      `Not_implemented
  | Internal ->
      `Internal_server_error
  | Unavailable ->
      `Service_unavailable
  | Data_loss ->
      `Internal_server_error
  | Unauthenticated ->
      `Unauthorized
