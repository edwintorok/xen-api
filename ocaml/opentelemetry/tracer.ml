module type IdGenerator = sig
  val generate_span_id_bytes : unit -> bytes

  val generate_trace_id_bytes : unit -> bytes
end

module DefaultIDGenerator = struct
  let generate_trace_id_bytes () =
    Uuidm.create `V4 |> Uuidm.to_bytes |> Bytes.of_string

  let generate_span_id_bytes () =
    let b = Bytes.create 8 in
    Bytes.set_int64_ne b 0 (Random.int64 Int64.max_int) ;
    b
end

module NoopGenerator = struct
  (* TODO: should be strings.. *)
  let generate_trace_id_bytes () = Bytes.make 16 '\x00'

  let generate_span_id_bytes () = Bytes.make 8 '\x00'
end

let get_trace_state ctx =
  ctx |> Context.get_active |> Option.map Span.get_trace_state

module Sampler = struct
  type t = bool

  type decision = DROP | RECORD_ONLY | RECORD_AND_SAMPLE

  let always_on = true

  let noop = false

  let should_sample ?context ~trace_id ~name ~kind ~attributes ~links t =
    if t then (* TODO: overridable *)
      (RECORD_AND_SAMPLE, [], Option.bind context get_trace_state)
    else
      (DROP, [], Option.bind context get_trace_state)
end

module type SpanProcessor = sig
  type t

  (* TODO: attributs on whether span is rw *)

  val on_start : t -> span:Span.t -> parent_context:Context.t -> unit

  val on_end : t -> span:Span.t -> unit

  val force_flush : t -> unit

  val shutdown : t -> unit
end

module type SpanProcessorInstance = sig
  module P : SpanProcessor

  val t : P.t
end

module Config = struct
  type t = {
      idgen: (module IdGenerator)
    ; sampler: Sampler.t
    ; mutable processors: (module SpanProcessorInstance) Queue.t option
  }

  let default () =
    {
      idgen= (module DefaultIDGenerator)
    ; sampler= Sampler.always_on
    ; processors= Some (Queue.create ())
    }

  let noop =
    {idgen= (module NoopGenerator); sampler= Sampler.noop; processors= None}
end

module T = struct
  type t = {
      config: Config.t
    ; lib: Proto.Common.V1.InstrumentationLibrary.t
    ; schema_url: string option
  }
end

include T

type exportable_spans = Proto.Trace.V1.ResourceSpans.t

module type SpanExporter = sig
  type t

  val export : t -> exportable_spans list -> bool

  val force_flush : t -> unit

  val shutdown : t -> unit
end

module SimpleProcessor (E : SpanExporter) = struct
  type t = {tracer: T.t; exporter: E.t}

  let create tracer exporter = {tracer; exporter}

  let get_raw span = span.Span.raw

  let is_sampled span = span.Span.sampled

  open Proto.Trace.V1

  let on_start t ~span ~parent_context = ()

  let on_end t ~span =
    if is_sampled span then
      let instrumentation_library_spans =
        [
          InstrumentationLibrarySpans.make ~instrumentation_library:t.tracer.lib
            ~spans:[get_raw span] ?schema_url:t.tracer.schema_url ()
        ]
      in
      (* TODO: resource *)
      (* TODO: handle dropping *)
      let (_ : bool) =
        [ResourceSpans.make ~instrumentation_library_spans ()]
        |> E.export t.exporter
      in
      ()

  let force_flush t = E.force_flush t.exporter

  let shutdown t = E.shutdown t.exporter
end

module BatchProcessor (E : SpanExporter) = struct
  type t = {exporter: E.t}

  let on_start t ~span ~parent_context = ()

  let on_end t ~span = failwith "TODO"

  let force_flush t = failwith "TODO"

  let shutdown t = E.shutdown t.exporter

  let create ?(max_queue_size = 2048) ?(scheduled_delay_millis = 5000)
      ?(export_timeout_millis = 30000) ?(max_export_batch_size = 512) t exporter
      =
    failwith "TODO"
end

module Provider = struct
  type t = Config.t

  let create () = Config.default ()

  let global = create ()

  let noop =
    T.
      {
        config= Config.noop
      ; lib= Proto.Common.V1.InstrumentationLibrary.make ~name:"builtin-noop" ()
      ; schema_url= None
      }
    

  let register (type a) t processor_module p =
    match t.Config.processors with
    | Some q ->
        let module M = struct
          module P = (val processor_module : SpanProcessor with type t = a)

          let t = p
        end in
        Queue.push (module M : SpanProcessorInstance) q
    | None ->
        ()
  (* TODO: log noop *)

  let get ~name ?version ?schema_url t =
    match t.Config.processors with
    | None ->
        noop (* shutdown *)
    | Some _ ->
        let lib =
          Proto.Common.V1.InstrumentationLibrary.make ~name ?version ()
        in
        T.{config= t; lib; schema_url}

  let forall (type a) t v f =
    Option.iter (Queue.iter @@ fun p -> f p v) t.Config.processors

  let shutdown t =
    (forall t () @@ fun (module P) () -> P.P.shutdown P.t) ;
    t.processors <- None

  let force_flush (type a) t =
    forall t () @@ fun (module P) () -> P.P.force_flush P.t

  let on_start (type a) t ~span ~parent_context =
    if span.Span.is_recording then
      forall t () @@ fun (module P) () -> P.P.on_start P.t ~span ~parent_context

  let on_end (type a) t ~span =
    if span.Span.is_recording then
      forall t () @@ fun (module P) () -> P.P.on_end P.t ~span
end

let get_span = Context.get_active

let add_span context (span, _) = Context.set_active context (Some span)

type span_kind = Proto.Trace.V1.Span.SpanKind.t

let get_parent ctx = Context.get_active ctx

let get_parent_trace_id ctx = ctx |> get_parent |> Option.map Span.get_trace_id

let get_parent_span_id ctx = ctx |> get_parent |> Option.map Span.get_span_id

let internal = Proto.Trace.V1.Span.SpanKind.SPAN_KIND_INTERNAL

let get_time_if_needed = function
  | Some t ->
      t
  | None ->
      Oclock.gettime Oclock.realtime

type attributes = Proto.Common.V1.KeyValue.t list

type links = Proto.Trace.V1.Span.Link.t list

type config = Config.t

type span = Span.t * config

let create_span ~name ~context ?(kind = internal) ?(attributes = [])
    ?(links = []) ?start_time_unix_nano t =
  let config = t.T.config in
  let module IDGen = (val config.Config.idgen) in
  let trace_id =
    match get_parent_trace_id context with
    | Some id ->
        id
    | None ->
        IDGen.generate_trace_id_bytes ()
  in
  let decision, extra_attributes, trace_state =
    Sampler.should_sample config.Config.sampler ~context ~trace_id ~name ~kind
      ~attributes ~links
  in
  let attributes = List.rev_append extra_attributes attributes in
  let is_recording, sampled =
    match decision with
    | DROP ->
        (false, false)
    | RECORD_ONLY ->
        (true, false)
    | RECORD_AND_SAMPLE ->
        (true, true)
  in
  (* SDK spec says to always generate this *)
  let span_id = IDGen.generate_span_id_bytes () in
  let parent_span_id = get_parent_span_id context in
  let start_time_unix_nano = get_time_if_needed start_time_unix_nano in
  let raw =
    Proto.Trace.V1.Span.make ~trace_id ~span_id ?trace_state ?parent_span_id
      ~name ~kind ~start_time_unix_nano ~attributes ~links ()
  in
  let span = Span.{raw; is_recording; sampled} in
  Provider.on_start config ~span ~parent_context:context ;
  (span, config)

let end_span ?end_time_unix_nano (span, config) =
  let open Span in
  let end_time_unix_nano = get_time_if_needed end_time_unix_nano in
  span.raw <- {span.raw with Proto.Trace.V1.Span.end_time_unix_nano} ;
  Provider.on_end config ~span ;
  span.is_recording <- false

let with_span ~name ~context ?kind ?attributes ?links t f =
  let span = create_span ~name ~context ?kind ?attributes ?links t in
  let finally () = end_span span in
  Fun.protect ~finally (fun () -> f span)

(* see
   https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md
   *)
