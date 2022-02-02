module Opentelemetry : sig
  module Proto : sig
    module Collector : sig
      module Trace : sig
        module V1 : sig
          module rec ExportTraceServiceRequest : sig
            val name' : unit -> string

            type t = Trace.Opentelemetry.Proto.Trace.V1.ResourceSpans.t list

            val make :
                 ?resource_spans:
                   Trace.Opentelemetry.Proto.Trace.V1.ResourceSpans.t list
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

          and ExportTraceServiceResponse : sig
            val name' : unit -> string

            type t = unit

            val make : unit -> t

            val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

            val from_proto :
                 Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          module TraceService : sig
            val export :
              (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                 with type t = ExportTraceServiceRequest.t
              )
              * (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                   with type t = ExportTraceServiceResponse.t
                )
          end
        end
      end
    end
  end
end
