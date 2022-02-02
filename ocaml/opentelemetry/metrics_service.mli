module Opentelemetry : sig
  module Proto : sig
    module Collector : sig
      module Metrics : sig
        module V1 : sig
          module rec ExportMetricsServiceRequest : sig
            val name' : unit -> string

            type t =
              Metrics.Opentelemetry.Proto.Metrics.V1.ResourceMetrics.t list

            val make :
                 ?resource_metrics:
                   Metrics.Opentelemetry.Proto.Metrics.V1.ResourceMetrics.t list
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

          and ExportMetricsServiceResponse : sig
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

          module MetricsService : sig
            val export :
              (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                 with type t = ExportMetricsServiceRequest.t
              )
              * (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                   with type t = ExportMetricsServiceResponse.t
                )
          end
        end
      end
    end
  end
end
