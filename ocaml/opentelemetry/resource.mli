module Opentelemetry : sig
  module Proto : sig
    module Resource : sig
      module V1 : sig
        module rec Resource : sig
          val name' : unit -> string

          type t = {
              attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; dropped_attributes_count: int
          }

          val make :
               ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?dropped_attributes_count:int
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
