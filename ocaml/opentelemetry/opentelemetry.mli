module Proto :
  sig
    module Common :
      sig
        module V1 = Common.Opentelemetry.Proto.Common.V1
      end
    module Resource :
      sig
        module V1 = Resource.Opentelemetry.Proto.Resource.V1
      end
    module Collector :
      sig
        module Metrics :
          sig
            module V1 =
              Metrics_service.Opentelemetry.Proto.Collector.Metrics.V1
          end
        module Logs :
          sig
            module V1 =
              Logs_service.Opentelemetry.Proto.Collector.Logs.V1
          end
        module Trace :
          sig
            module V1 =
              Trace_service.Opentelemetry.Proto.Collector.Trace.V1
          end
      end
    module Metrics :
      sig
        module V1 = Metrics.Opentelemetry.Proto.Metrics.V1
        module Experimental =
          Metrics_config_service.Opentelemetry.Proto.Metrics.Experimental
      end
    module Logs :
      sig module V1 = Logs.Opentelemetry.Proto.Logs.V1 end
    module Trace :
      sig
        module V1 :
          sig
            module TraceConfig =
              Trace_config.Opentelemetry.Proto.Trace.V1.TraceConfig
            module ConstantSampler =
              Trace_config.Opentelemetry.Proto.Trace.V1.ConstantSampler
            module TraceIdRatioBased =
              Trace_config.Opentelemetry.Proto.Trace.V1.TraceIdRatioBased
            module RateLimitingSampler =
              Trace_config.Opentelemetry.Proto.Trace.V1.RateLimitingSampler
            module TracesData =
              Trace.Opentelemetry.Proto.Trace.V1.TracesData
            module ResourceSpans =
              Trace.Opentelemetry.Proto.Trace.V1.ResourceSpans
            module InstrumentationLibrarySpans =
              Trace.Opentelemetry.Proto.Trace.V1.InstrumentationLibrarySpans
            module Span =
              Trace.Opentelemetry.Proto.Trace.V1.Span
            module Status =
              Trace.Opentelemetry.Proto.Trace.V1.Status
          end
      end
  end
