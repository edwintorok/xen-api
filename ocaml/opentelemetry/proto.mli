module Common : sig
  module V1 : sig
    module rec AnyValue : sig
      val name' : unit -> string

      type t =
        [ `Array_value of ArrayValue.t
        | `Bool_value of bool
        | `Bytes_value of bytes
        | `Double_value of float
        | `Int_value of int
        | `Kvlist_value of KeyValueList.t
        | `String_value of string
        | `not_set ]

      val make :
           ?value:
             [ `Array_value of ArrayValue.t
             | `Bool_value of bool
             | `Bytes_value of bytes
             | `Double_value of float
             | `Int_value of int
             | `Kvlist_value of KeyValueList.t
             | `String_value of string
             | `not_set ]
        -> unit
        -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and ArrayValue : sig
      val name' : unit -> string

      type t = AnyValue.t list

      val make : ?values:AnyValue.t list -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and KeyValueList : sig
      val name' : unit -> string

      type t = KeyValue.t list

      val make : ?values:KeyValue.t list -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and KeyValue : sig
      val name' : unit -> string

      type t = {key: string; value: AnyValue.t option}

      val make : ?key:string -> ?value:AnyValue.t -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and StringKeyValue : sig
      val name' : unit -> string

      type t = {key: string; value: string}

      val make : ?key:string -> ?value:string -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and InstrumentationLibrary : sig
      val name' : unit -> string

      type t = {name: string; version: string}

      val make : ?name:string -> ?version:string -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end
  end
end

module Resource : sig
  module V1 : sig
    module rec Resource : sig
      val name' : unit -> string

      type t = {
          attributes: Common.V1.KeyValue.t list
        ; dropped_attributes_count: int
      }

      val make :
           ?attributes:Common.V1.KeyValue.t list
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

module Metrics : sig
  module V1 : sig
    module rec AggregationTemporality : sig
      type t =
        | AGGREGATION_TEMPORALITY_UNSPECIFIED
        | AGGREGATION_TEMPORALITY_DELTA
        | AGGREGATION_TEMPORALITY_CUMULATIVE

      val to_int : t -> int

      val from_int :
        int -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and DataPointFlags : sig
      type t = FLAG_NONE | FLAG_NO_RECORDED_VALUE

      val to_int : t -> int

      val from_int :
        int -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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
          resource: Resource.V1.Resource.t option
        ; instrumentation_library_metrics: InstrumentationLibraryMetrics.t list
        ; schema_url: string
      }

      val make :
           ?resource:Resource.V1.Resource.t
        -> ?instrumentation_library_metrics:InstrumentationLibraryMetrics.t list
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
          instrumentation_library: Common.V1.InstrumentationLibrary.t option
        ; metrics: Metric.t list
        ; schema_url: string
      }

      val make :
           ?instrumentation_library:Common.V1.InstrumentationLibrary.t
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
          attributes: Common.V1.KeyValue.t list
        ; start_time_unix_nano: int64
        ; time_unix_nano: int64
        ; value: [`As_double of float | `As_int of int64 | `not_set]
        ; exemplars: Exemplar.t list
        ; flags: int
      }

      val make :
           ?attributes:Common.V1.KeyValue.t list
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
          attributes: Common.V1.KeyValue.t list
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
           ?attributes:Common.V1.KeyValue.t list
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
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      val name' : unit -> string

      type t = {
          attributes: Common.V1.KeyValue.t list
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
           ?attributes:Common.V1.KeyValue.t list
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
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      val name' : unit -> string

      type t = {
          attributes: Common.V1.KeyValue.t list
        ; start_time_unix_nano: int64
        ; time_unix_nano: int64
        ; count: int64
        ; sum: float
        ; quantile_values: SummaryDataPoint.ValueAtQuantile.t list
        ; flags: int
      }

      val make :
           ?attributes:Common.V1.KeyValue.t list
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
          filtered_attributes: Common.V1.KeyValue.t list
        ; time_unix_nano: int64
        ; value: [`As_double of float | `As_int of int64 | `not_set]
        ; span_id: bytes
        ; trace_id: bytes
      }

      val make :
           ?filtered_attributes:Common.V1.KeyValue.t list
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

  module Experimental : sig
    module rec MetricConfigRequest : sig
      val name' : unit -> string

      type t = {
          resource: Resource.V1.Resource.t option
        ; last_known_fingerprint: bytes
      }

      val make :
           ?resource:Resource.V1.Resource.t
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
               ?match':[`Equals of string | `Starts_with of string | `not_set]
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        val name' : unit -> string

        type t = {
            exclusion_patterns: MetricConfigResponse.Schedule.Pattern.t list
          ; inclusion_patterns: MetricConfigResponse.Schedule.Pattern.t list
          ; period_sec: int
        }

        val make :
             ?exclusion_patterns:MetricConfigResponse.Schedule.Pattern.t list
          -> ?inclusion_patterns:MetricConfigResponse.Schedule.Pattern.t list
          -> ?period_sec:int
          -> unit
          -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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
        int -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

    and LogRecordFlags : sig
      type t = LOG_RECORD_FLAG_UNSPECIFIED | LOG_RECORD_FLAG_TRACE_FLAGS_MASK

      val to_int : t -> int

      val from_int :
        int -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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
          resource: Resource.V1.Resource.t option
        ; instrumentation_library_logs: InstrumentationLibraryLogs.t list
        ; schema_url: string
      }

      val make :
           ?resource:Resource.V1.Resource.t
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
          instrumentation_library: Common.V1.InstrumentationLibrary.t option
        ; log_records: LogRecord.t list
        ; schema_url: string
      }

      val make :
           ?instrumentation_library:Common.V1.InstrumentationLibrary.t
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
        ; body: Common.V1.AnyValue.t option
        ; attributes: Common.V1.KeyValue.t list
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
        -> ?body:Common.V1.AnyValue.t
        -> ?attributes:Common.V1.KeyValue.t list
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
          resource: Resource.V1.Resource.t option
        ; instrumentation_library_spans: InstrumentationLibrarySpans.t list
        ; schema_url: string
      }

      val make :
           ?resource:Resource.V1.Resource.t
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
          instrumentation_library: Common.V1.InstrumentationLibrary.t option
        ; spans: Span.t list
        ; schema_url: string
      }

      val make :
           ?instrumentation_library:Common.V1.InstrumentationLibrary.t
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
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      and Event : sig
        val name' : unit -> string

        type t = {
            time_unix_nano: int64
          ; name: string
          ; attributes: Common.V1.KeyValue.t list
          ; dropped_attributes_count: int
        }

        val make :
             ?time_unix_nano:int64
          -> ?name:string
          -> ?attributes:Common.V1.KeyValue.t list
          -> ?dropped_attributes_count:int
          -> unit
          -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      and Link : sig
        val name' : unit -> string

        type t = {
            trace_id: bytes
          ; span_id: bytes
          ; trace_state: string
          ; attributes: Common.V1.KeyValue.t list
          ; dropped_attributes_count: int
        }

        val make :
             ?trace_id:bytes
          -> ?span_id:bytes
          -> ?trace_state:string
          -> ?attributes:Common.V1.KeyValue.t list
          -> ?dropped_attributes_count:int
          -> unit
          -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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
        ; attributes: Common.V1.KeyValue.t list
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
        -> ?attributes:Common.V1.KeyValue.t list
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
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      val name' : unit -> string

      type t = {message: string; code: Status.StatusCode.t}

      val make : ?message:string -> ?code:Status.StatusCode.t -> unit -> t

      val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

      val from_proto :
           Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
        -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
    end

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
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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

module Collector : sig
  module Metrics : sig
    module V1 : sig
      module rec ExportMetricsServiceRequest : sig
        val name' : unit -> string

        type t = Metrics.V1.ResourceMetrics.t list

        val make :
          ?resource_metrics:Metrics.V1.ResourceMetrics.t list -> unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      and ExportMetricsServiceResponse : sig
        val name' : unit -> string

        type t = unit

        val make : unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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

  module Logs : sig
    module V1 : sig
      module rec ExportLogsServiceRequest : sig
        val name' : unit -> string

        type t = Logs.V1.ResourceLogs.t list

        val make : ?resource_logs:Logs.V1.ResourceLogs.t list -> unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      and ExportLogsServiceResponse : sig
        val name' : unit -> string

        type t = unit

        val make : unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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

  module Trace : sig
    module V1 : sig
      module rec ExportTraceServiceRequest : sig
        val name' : unit -> string

        type t = Trace.V1.ResourceSpans.t list

        val make : ?resource_spans:Trace.V1.ResourceSpans.t list -> unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
      end

      and ExportTraceServiceResponse : sig
        val name' : unit -> string

        type t = unit

        val make : unit -> t

        val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

        val from_proto :
             Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
          -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
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
