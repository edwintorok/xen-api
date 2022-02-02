module Opentelemetry : sig
  module Proto : sig
    module Metrics : sig
      module Experimental : sig
        module rec MetricConfigRequest : sig
          val name' : unit -> string

          type t = {
              resource:
                Resource.Opentelemetry.Proto.Resource.V1.Resource.t option
            ; last_known_fingerprint: bytes
          }

          val make :
               ?resource:Resource.Opentelemetry.Proto.Resource.V1.Resource.t
            -> ?last_known_fingerprint:bytes
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and MetricConfigResponse : sig
          module rec Schedule : sig
            module rec Pattern : sig
              val name' : unit -> string

              type t = [`Equals of string | `Starts_with of string | `not_set]

              val make :
                   ?match':
                     [`Equals of string | `Starts_with of string | `not_set]
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
                exclusion_patterns: MetricConfigResponse.Schedule.Pattern.t list
              ; inclusion_patterns: MetricConfigResponse.Schedule.Pattern.t list
              ; period_sec: int
            }

            val make :
                 ?exclusion_patterns:MetricConfigResponse.Schedule.Pattern.t list
              -> ?inclusion_patterns:
                   MetricConfigResponse.Schedule.Pattern.t list
              -> ?period_sec:int
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
              fingerprint: bytes
            ; schedules: MetricConfigResponse.Schedule.t list
            ; suggested_wait_time_sec: int
          }

          val make :
               ?fingerprint:bytes
            -> ?schedules:MetricConfigResponse.Schedule.t list
            -> ?suggested_wait_time_sec:int
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        module MetricConfig : sig
          val getMetricConfig :
            (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
               with type t = MetricConfigRequest.t
            )
            * (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                 with type t = MetricConfigResponse.t
              )
        end
      end
    end
  end
end
