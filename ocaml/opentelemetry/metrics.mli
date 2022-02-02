module Opentelemetry : sig
  module Proto : sig
    module Metrics : sig
      module V1 : sig
        module rec AggregationTemporality : sig
          type t =
            | AGGREGATION_TEMPORALITY_UNSPECIFIED
            | AGGREGATION_TEMPORALITY_DELTA
            | AGGREGATION_TEMPORALITY_CUMULATIVE

          val to_int : t -> int

          val from_int :
               int
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and DataPointFlags : sig
          type t = FLAG_NONE | FLAG_NO_RECORDED_VALUE

          val to_int : t -> int

          val from_int :
               int
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and MetricsData : sig
          val name' : unit -> string

          type t = ResourceMetrics.t list

          val make : ?resource_metrics:ResourceMetrics.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ResourceMetrics : sig
          val name' : unit -> string

          type t = {
              resource:
                Resource.Opentelemetry.Proto.Resource.V1.Resource.t option
            ; instrumentation_library_metrics:
                InstrumentationLibraryMetrics.t list
            ; schema_url: string
          }

          val make :
               ?resource:Resource.Opentelemetry.Proto.Resource.V1.Resource.t
            -> ?instrumentation_library_metrics:
                 InstrumentationLibraryMetrics.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and InstrumentationLibraryMetrics : sig
          val name' : unit -> string

          type t = {
              instrumentation_library:
                Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
                option
            ; metrics: Metric.t list
            ; schema_url: string
          }

          val make :
               ?instrumentation_library:
                 Common.Opentelemetry.Proto.Common.V1.InstrumentationLibrary.t
            -> ?metrics:Metric.t list
            -> ?schema_url:string
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Metric : sig
          val name' : unit -> string

          type t = {
              name: string
            ; description: string
            ; unit: string
            ; data:
                [ `Exponential_histogram of ExponentialHistogram.t
                | `Gauge of Gauge.t
                | `Histogram of Histogram.t
                | `Sum of Sum.t
                | `Summary of Summary.t
                | `not_set ]
          }

          val make :
               ?name:string
            -> ?description:string
            -> ?unit:string
            -> ?data:
                 [ `Exponential_histogram of ExponentialHistogram.t
                 | `Gauge of Gauge.t
                 | `Histogram of Histogram.t
                 | `Sum of Sum.t
                 | `Summary of Summary.t
                 | `not_set ]
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Gauge : sig
          val name' : unit -> string

          type t = NumberDataPoint.t list

          val make : ?data_points:NumberDataPoint.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Sum : sig
          val name' : unit -> string

          type t = {
              data_points: NumberDataPoint.t list
            ; aggregation_temporality: AggregationTemporality.t
            ; is_monotonic: bool
          }

          val make :
               ?data_points:NumberDataPoint.t list
            -> ?aggregation_temporality:AggregationTemporality.t
            -> ?is_monotonic:bool
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Histogram : sig
          val name' : unit -> string

          type t = {
              data_points: HistogramDataPoint.t list
            ; aggregation_temporality: AggregationTemporality.t
          }

          val make :
               ?data_points:HistogramDataPoint.t list
            -> ?aggregation_temporality:AggregationTemporality.t
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ExponentialHistogram : sig
          val name' : unit -> string

          type t = {
              data_points: ExponentialHistogramDataPoint.t list
            ; aggregation_temporality: AggregationTemporality.t
          }

          val make :
               ?data_points:ExponentialHistogramDataPoint.t list
            -> ?aggregation_temporality:AggregationTemporality.t
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Summary : sig
          val name' : unit -> string

          type t = SummaryDataPoint.t list

          val make : ?data_points:SummaryDataPoint.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and NumberDataPoint : sig
          val name' : unit -> string

          type t = {
              attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; start_time_unix_nano: int64
            ; time_unix_nano: int64
            ; value: [`As_double of float | `As_int of int64 | `not_set]
            ; exemplars: Exemplar.t list
            ; flags: int
          }

          val make :
               ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?start_time_unix_nano:int64
            -> ?time_unix_nano:int64
            -> ?value:[`As_double of float | `As_int of int64 | `not_set]
            -> ?exemplars:Exemplar.t list
            -> ?flags:int
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and HistogramDataPoint : sig
          val name' : unit -> string

          type t = {
              attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; start_time_unix_nano: int64
            ; time_unix_nano: int64
            ; count: int64
            ; sum: float
            ; bucket_counts: int64 list
            ; explicit_bounds: float list
            ; exemplars: Exemplar.t list
            ; flags: int
          }

          val make :
               ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?start_time_unix_nano:int64
            -> ?time_unix_nano:int64
            -> ?count:int64
            -> ?sum:float
            -> ?bucket_counts:int64 list
            -> ?explicit_bounds:float list
            -> ?exemplars:Exemplar.t list
            -> ?flags:int
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ExponentialHistogramDataPoint : sig
          module rec Buckets : sig
            val name' : unit -> string

            type t = {offset: int; bucket_counts: int list}

            val make : ?offset:int -> ?bucket_counts:int list -> unit -> t

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
              attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; start_time_unix_nano: int64
            ; time_unix_nano: int64
            ; count: int64
            ; sum: float
            ; scale: int
            ; zero_count: int64
            ; positive: ExponentialHistogramDataPoint.Buckets.t option
            ; negative: ExponentialHistogramDataPoint.Buckets.t option
            ; flags: int
            ; exemplars: Exemplar.t list
          }

          val make :
               ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?start_time_unix_nano:int64
            -> ?time_unix_nano:int64
            -> ?count:int64
            -> ?sum:float
            -> ?scale:int
            -> ?zero_count:int64
            -> ?positive:ExponentialHistogramDataPoint.Buckets.t
            -> ?negative:ExponentialHistogramDataPoint.Buckets.t
            -> ?flags:int
            -> ?exemplars:Exemplar.t list
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and SummaryDataPoint : sig
          module rec ValueAtQuantile : sig
            val name' : unit -> string

            type t = {quantile: float; value: float}

            val make : ?quantile:float -> ?value:float -> unit -> t

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
              attributes: Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; start_time_unix_nano: int64
            ; time_unix_nano: int64
            ; count: int64
            ; sum: float
            ; quantile_values: SummaryDataPoint.ValueAtQuantile.t list
            ; flags: int
          }

          val make :
               ?attributes:Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?start_time_unix_nano:int64
            -> ?time_unix_nano:int64
            -> ?count:int64
            -> ?sum:float
            -> ?quantile_values:SummaryDataPoint.ValueAtQuantile.t list
            -> ?flags:int
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and Exemplar : sig
          val name' : unit -> string

          type t = {
              filtered_attributes:
                Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            ; time_unix_nano: int64
            ; value: [`As_double of float | `As_int of int64 | `not_set]
            ; span_id: bytes
            ; trace_id: bytes
          }

          val make :
               ?filtered_attributes:
                 Common.Opentelemetry.Proto.Common.V1.KeyValue.t list
            -> ?time_unix_nano:int64
            -> ?value:[`As_double of float | `As_int of int64 | `not_set]
            -> ?span_id:bytes
            -> ?trace_id:bytes
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
