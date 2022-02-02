module type IdGenerator = sig
  val generate_span_id_bytes : unit -> bytes
  val generate_trace_id_bytes: unit -> bytes
end

module DefaultIDGenerator = struct
  let generate_trace_id_bytes () =
    Uuidm.create `V4 |> Uuidm.to_bytes |> Bytes.of_string

  let generate_span_id_bytes () =
    let b = Bytes.create 8 in
    Bytes.set_int64_ne b 0 (Random.int64 Int64.max_int);
    b

end

module Sampler = struct
  type t = unit

  type decision = DROP | RECORD_ONLY | RECORD_AND_SAMPLE

  let should_sample ?context ~trace_id ~name ~kind ~attributes ~links t =
    RECORD_AND_SAMPLE, [], Context.get_trace_state context

  let default = ()
end

module Config = struct
  type t = {
    idgen: (module IdGenerator)
  ; sampler: Sampler.t
  }

  let default = { idgen = (module DefaultIDGenerator); sampler = Sampler.default }
end

module T = struct
  type t =
    { config: Config.t
    ; lib: Proto.Common.V1.InstrumentationLibrary.t
    ; schema_url: string option
  }

end

type t = T.t option

module SpanProcessor = struct
  type t

  let force_flush t = ()
  let shutdown t = ()
end

module Provider = struct
  type t = {
    config: Config.t
  ; mutable processors: SpanProcessor.t list option
  }

  let global = failwith "TODO"
  let create () = failwith "TODO"
  let get ~name ?version ?schema_url t =
    match t.processors with
    | None -> None (* shutdown *)
    | Some _ ->
    let lib = Proto.Common.V1.InstrumentationLibrary.make ~name ?version () in
    Some T.{ config = t.config; lib; schema_url }

  let shutdown t =
    Option.iter (List.iter SpanProcessor.shutdown) t.processors;
    t.processors <- None

  let force_flush t =
    Option.iter (List.iter SpanProcessor.force_flush) t.processors;
end

let get_span context = failwith "TODO"
let add_span context = failwith "TODO"

type span_kind = Proto.Trace.V1.Span.SpanKind.t

let encoder, decoder = Ocaml_protoc_plugin.Service.make_client_functions
    Proto.Collector.Trace.V1.TraceService.export

let send_spans resource_spans f =
  Proto.Collector.Trace.V1.ExportTraceServiceRequest.make ~resource_spans ()
  |> encoder |> Ocaml_protoc_plugin.Writer.contents
  |> f

let to_result = function
  | Ok v -> Ok v
  | Error e -> Error (`Msg (Ocaml_protoc_plugin.Result.show_error e))

let parse_reply reply =
  reply |> Ocaml_protoc_plugin.Reader.create
  |> decoder
  |> to_result

let parse_reply_exn reply =
  match parse_reply reply with
  | Ok v -> v
  | Error (`Msg m) -> failwith m

let get_parent_trace_id ctx = failwith "TODO"
let get_parent_span_id ctx = failwith "TODO"
let internal = Proto.Trace.V1.Span.SpanKind.SPAN_KIND_INTERNAL

module Span = struct
  type t =
    { mutable raw: Proto.Trace.V1.Span.t
    ; mutable is_recording: bool
    ; sampled: bool
  }
end

let get_time_if_needed = function
  | Some t -> t
  | None -> Oclock.gettime Oclock.realtime

let create_span ~name ~context ?(kind = internal) ?(attributes=[]) ?(links=[]) ?start_time_unix_nano t =
  let config = t.T.config in
  let module IDGen = (val config.Config.idgen) in
  let trace_id = match get_parent_trace_id context with
    | Some id -> id
    | None ->
      IDGen.generate_trace_id_bytes ()
   in
   let decision, attributes, trace_state = Sampler.should_sample config.Config.sampler ~context ~trace_id
   ~name ~kind ~attributes ~links
   in
   let is_recording, sampled = match decision with
     | DROP -> false, false
     | RECORD_ONLY -> true, false
     | RECORD_AND_SAMPLE -> true, true
   in
   (* SDK spec says to always generate this *)
   let span_id = IDGen.generate_span_id_bytes () in
   let parent_span_id = get_parent_span_id context in
   let start_time_unix_nano = get_time_if_needed start_time_unix_nano in
   let raw = Proto.Trace.V1.Span.make
     ~trace_id ~span_id ~trace_state ?parent_span_id
     ~name ~kind ~start_time_unix_nano ~attributes ~links () in
   Span.{ raw; is_recording; sampled }

let end_span ?end_time_unix_nano span =
  let open Span in
  let end_time_unix_nano = get_time_if_needed end_time_unix_nano in
  span.raw <- { span.raw with Proto.Trace.V1.Span.end_time_unix_nano };
  span.is_recording <- false

let with_span ~name ~context ?kind ?attributes ?links t f =
  let span = create_span ~name ~context ?kind ?attributes ?links t in
  let finally () = end_span span in
  Fun.protect ~finally f span
