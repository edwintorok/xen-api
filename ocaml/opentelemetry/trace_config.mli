module Opentelemetry : sig
  module Proto : sig
    module Trace : sig
      module V1 : sig
        module rec TraceConfig : sig
          val name' : unit -> string

          type t = {
              sampler:
                [ `Constant_sampler of ConstantSampler.t
                | `Rate_limiting_sampler of RateLimitingSampler.t
                | `Trace_id_ratio_based of TraceIdRatioBased.t
                | `not_set ]
            ; max_number_of_attributes: int
            ; max_number_of_timed_events: int
            ; max_number_of_attributes_per_timed_event: int
            ; max_number_of_links: int
            ; max_number_of_attributes_per_link: int
          }

          val make :
               ?sampler:
                 [ `Constant_sampler of ConstantSampler.t
                 | `Rate_limiting_sampler of RateLimitingSampler.t
                 | `Trace_id_ratio_based of TraceIdRatioBased.t
                 | `not_set ]
            -> ?max_number_of_attributes:int
            -> ?max_number_of_timed_events:int
            -> ?max_number_of_attributes_per_timed_event:int
            -> ?max_number_of_links:int
            -> ?max_number_of_attributes_per_link:int
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ConstantSampler : sig
          module rec ConstantDecision : sig
            type t = ALWAYS_OFF | ALWAYS_ON | ALWAYS_PARENT

            val to_int : t -> int

            val from_int :
                 int
              -> ( t
                 , [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]
                 )
                 result
          end

          val name' : unit -> string

          type t = ConstantSampler.ConstantDecision.t

          val make : ?decision:ConstantSampler.ConstantDecision.t -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and TraceIdRatioBased : sig
          val name' : unit -> string

          type t = float

          val make : ?samplingRatio:float -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and RateLimitingSampler : sig
          val name' : unit -> string

          type t = int

          val make : ?qps:int -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end
      end
    end
  end
end
