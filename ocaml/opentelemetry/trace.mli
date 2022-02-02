module Opentelemetry : sig
  module Proto : sig
    module Trace : sig
      module V1 : sig
        module rec TracesData : sig
          val name' : unit -> string

          type t = ResourceSpans.t list

          val make : ?resource_spans:ResourceSpans.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ResourceSpans : sig
          val name' : unit -> string

          type t = {
              resource:
                Resource.Opentelemetry.Proto.Resource.V1.Resource.t option
            ; instrumentation_library_spans: InstrumentationLibrarySpans.t list
            ; schema_url: string
          }

          val make :
               ?resource:Resource.Opentelemetry.Proto.Resource.V1.Resource.t
            -> ?instrumentation_library_spans:InstrumentationLibrarySpans.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and InstrumentationLibrarySpans : sig
          val name' : unit -> string

          type t = {
              instrumentation_library:
                Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
                option
            ; spans: Span.t list
            ; schema_url: string
          }

          val make :
               ?instrumentation_library:
                 Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
            -> ?spans:Span.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Span : sig
          module rec SpanKind : sig
            type t =
              | SPAN_KIND_UNSPECIFIED
              | SPAN_KIND_INTERNAL
              | SPAN_KIND_SERVER
              | SPAN_KIND_CLIENT
              | SPAN_KIND_PRODUCER
              | SPAN_KIND_CONSUMER

            val to_int : t -> int

            val from_int :
                 int
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          and Event : sig
            val name' : unit -> string

            type t = {
                time_unix_nano: int64
              ; name: string
              ; attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
              ; dropped_attributes_count: int
            }

            val make :
                 ?time_unix_nano:int64
              -> ?name:string
              -> ?attributes:
                   Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
              -> ?dropped_attributes_count:int
              -> unit
              -> t

            val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

            val from_proto :
                 Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          and Link : sig
            val name' : unit -> string

            type t = {
                trace_id: bytes
              ; span_id: bytes
              ; trace_state: string
              ; attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
              ; dropped_attributes_count: int
            }

            val make :
                 ?trace_id:bytes
              -> ?span_id:bytes
              -> ?trace_state:string
              -> ?attributes:
                   Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
              -> ?dropped_attributes_count:int
              -> unit
              -> t

            val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

            val from_proto :
                 Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          val name' : unit -> string

          type t = {
              trace_id: bytes
            ; span_id: bytes
            ; trace_state: string
            ; parent_span_id: bytes
            ; name: string
            ; kind: Span.SpanKind.t
            ; start_time_unix_nano: int64
            ; end_time_unix_nano: int64
            ; attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; dropped_attributes_count: int
            ; events: Span.Event.t list
            ; dropped_events_count: int
            ; links: Span.Link.t list
            ; dropped_links_count: int
            ; status: Status.t option
          }

          val make :
               ?trace_id:bytes
            -> ?span_id:bytes
            -> ?trace_state:string
            -> ?parent_span_id:bytes
            -> ?name:string
            -> ?kind:Span.SpanKind.t
            -> ?start_time_unix_nano:int64
            -> ?end_time_unix_nano:int64
            -> ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?dropped_attributes_count:int
            -> ?events:Span.Event.t list
            -> ?dropped_events_count:int
            -> ?links:Span.Link.t list
            -> ?dropped_links_count:int
            -> ?status:Status.t
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Status : sig
          module rec StatusCode : sig
            type t = STATUS_CODE_UNSET | STATUS_CODE_OK | STATUS_CODE_ERROR

            val to_int : t -> int

            val from_int :
                 int
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          val name' : unit -> string

          type t = {message: string; code: Status.StatusCode.t}

          val make : ?message:string -> ?code:Status.StatusCode.t -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end
      end
    end
  end
end
