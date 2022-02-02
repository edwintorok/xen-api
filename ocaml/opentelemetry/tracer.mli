module T: sig
  type t (** a Tracer *)
end
include module type of T

module Provider : sig
  type t
  (** a TracerProvider *)

  val global: t
  (** the global TracerProvider. If needed more TracerProviders can be created *)

  val create: unit -> t
  (** [create ()] creates a new TracerProvider *)

  val get: name:string -> ?version:string -> ?schema_url:string -> t -> T.t
  (** [get ~name ?version ?schema_url provider] returns a tracer *)
end

val get_span: Context.t -> Span.t
(** [get_span context] extracts the span from a [context] *)

val add_span: Context.t -> Span.t -> unit
(** [add_span context] adds a span to the [context] *)

(* TODO: for Lwt we could add support for an implicit context *)

type span_kind = Proto.Trace.V1.Span.SpanKind.t
type attributes = Proto.Common.V1.KeyValue.t list

val create_span: t -> name:string -> context:Context.t -> ?span_kind:span_kind ->
  ?attributes:attributes -> ?links:Span.Link.t list -> ?start_ns:int64 -> Span.t
(** [create_span tracer] creates a span using [tracer].
    This is the only API to create spans *)
