module T : sig
  (** a Tracer *)
  type t
end

type t = T.t

module type SpanProcessor = sig
  type t

  (* TODO: attributs on whether span is rw *)

  val on_start : t -> span:Span.t -> parent_context:Context.t -> unit

  val on_end : t -> span:Span.t -> unit

  val force_flush : t -> unit

  val shutdown : t -> unit
end

type exportable_spans = Proto.Trace.V1.ResourceSpans.t

module type SpanExporter = sig
  type t

  val export : t -> exportable_spans list -> bool

  val force_flush : t -> unit

  val shutdown : t -> unit
end

module TestExporter : sig
  include SpanExporter

  val create : unit -> t
end

module SimpleProcessor (E : SpanExporter) : sig
  include SpanProcessor

  val create : T.t -> E.t -> t
end

module BatchProcessor (E : SpanExporter) : sig
  include SpanProcessor

  val create :
       ?max_queue_size:int
    -> ?scheduled_delay_millis:int
    -> ?export_timeout_millis:int
    -> ?max_export_batch_size:int
    -> T.t
    -> E.t
    -> t
end

module Provider : sig
  (** a TracerProvider *)
  type t

  val global : t
  (** the global TracerProvider. If needed more TracerProviders can be created *)

  val create : unit -> t
  (** [create ()] creates a new TracerProvider *)

  val get : name:string -> ?version:string -> ?schema_url:string -> t -> T.t
  (** [get ~name ?version ?schema_url provider] returns a tracer *)

  val shutdown : t -> unit

  val force_flush : t -> unit

  val register : t -> (module SpanProcessor with type t = 'a) -> 'a -> unit
end

val get_span : Context.t -> Span.t option
(** [get_span context] extracts the span from a [context] *)

val add_span : Context.t -> Span.t -> Context.t
(** [add_span context] adds a span to the [context] *)

(* TODO: for Lwt we could add support for an implicit context *)

type span_kind = Proto.Trace.V1.Span.SpanKind.t

type attributes = Proto.Common.V1.KeyValue.t list

type links = Proto.Trace.V1.Span.Link.t list

type config

type span = Span.t * config

val create_span :
     name:string
  -> context:Context.t
  -> ?kind:span_kind
  -> ?attributes:attributes
  -> ?links:links
  -> ?start_time_unix_nano:int64
  -> t
  -> span
(** [create_span tracer] creates a span using [tracer].
    This is the only API to create spans *)

val end_span : ?end_time_unix_nano:int64 -> span -> unit
(** [end_span ?end_time_unix_nano span] *)

val with_span :
     name:string
  -> context:Context.t
  -> ?kind:span_kind
  -> ?attributes:attributes
  -> ?links:links
  -> t
  -> (span -> 'a)
  -> 'a
(** [with_span ~name ~context f] *)

(* see
   https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/api.md
   *)
