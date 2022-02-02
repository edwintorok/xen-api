module Opentelemetry : sig
  module Proto : sig
    module Collector : sig
      module Logs : sig
        module V1 : sig
          module rec ExportLogsServiceRequest : sig
            val name' : unit -> string

            type t = Logs.Opentelemetry.Proto.Logs.V1.ResourceLogs.t list

            val make :
                 ?resource_logs:
                   Logs.Opentelemetry.Proto.Logs.V1.ResourceLogs.t list
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

          and ExportLogsServiceResponse : sig
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

          module LogsService : sig
            val export :
              (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                 with type t = ExportLogsServiceRequest.t
              )
              * (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
                   with type t = ExportLogsServiceResponse.t
                )
          end
        end
      end
    end
  end
end
