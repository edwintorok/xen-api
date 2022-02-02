module Opentelemetry : sig
  module Proto : sig
    module Logs : sig
      module V1 : sig
        module rec SeverityNumber : sig
          type t =
            | SEVERITY_NUMBER_UNSPECIFIED
            | SEVERITY_NUMBER_TRACE
            | SEVERITY_NUMBER_TRACE2
            | SEVERITY_NUMBER_TRACE3
            | SEVERITY_NUMBER_TRACE4
            | SEVERITY_NUMBER_DEBUG
            | SEVERITY_NUMBER_DEBUG2
            | SEVERITY_NUMBER_DEBUG3
            | SEVERITY_NUMBER_DEBUG4
            | SEVERITY_NUMBER_INFO
            | SEVERITY_NUMBER_INFO2
            | SEVERITY_NUMBER_INFO3
            | SEVERITY_NUMBER_INFO4
            | SEVERITY_NUMBER_WARN
            | SEVERITY_NUMBER_WARN2
            | SEVERITY_NUMBER_WARN3
            | SEVERITY_NUMBER_WARN4
            | SEVERITY_NUMBER_ERROR
            | SEVERITY_NUMBER_ERROR2
            | SEVERITY_NUMBER_ERROR3
            | SEVERITY_NUMBER_ERROR4
            | SEVERITY_NUMBER_FATAL
            | SEVERITY_NUMBER_FATAL2
            | SEVERITY_NUMBER_FATAL3
            | SEVERITY_NUMBER_FATAL4

          val to_int : t -> int

          val from_int :
               int
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and LogRecordFlags : sig
          type t =
            | LOG_RECORD_FLAG_UNSPECIFIED
            | LOG_RECORD_FLAG_TRACE_FLAGS_MASK

          val to_int : t -> int

          val from_int :
               int
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and LogsData : sig
          val name' : unit -> string

          type t = ResourceLogs.t list

          val make : ?resource_logs:ResourceLogs.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ResourceLogs : sig
          val name' : unit -> string

          type t = {
              resource:
                Resource.Opentelemetry.Proto.Resource.V1.Resource.t option
            ; instrumentation_library_logs: InstrumentationLibraryLogs.t list
            ; schema_url: string
          }

          val make :
               ?resource:Resource.Opentelemetry.Proto.Resource.V1.Resource.t
            -> ?instrumentation_library_logs:InstrumentationLibraryLogs.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and InstrumentationLibraryLogs : sig
          val name' : unit -> string

          type t = {
              instrumentation_library:
                Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
                option
            ; log_records: LogRecord.t list
            ; schema_url: string
          }

          val make :
               ?instrumentation_library:
                 Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
            -> ?log_records:LogRecord.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and LogRecord : sig
          val name' : unit -> string

          type t = {
              time_unix_nano: int64
            ; severity_number: SeverityNumber.t
            ; severity_text: string
            ; name: string
            ; body: Common.Opentelemetry.Proto.Common.V1.AnyValue.t option
            ; attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; dropped_attributes_count: int
            ; flags: int32
            ; trace_id: bytes
            ; span_id: bytes
          }

          val make :
               ?time_unix_nano:int64
            -> ?severity_number:SeverityNumber.t
            -> ?severity_text:string
            -> ?name:string
            -> ?body:Common.Opentelemetry.Proto.Common.V1.AnyValue.t
            -> ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?dropped_attributes_count:int
            -> ?flags:int32
            -> ?trace_id:bytes
            -> ?span_id:bytes
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end
      end
    end
  end
end
