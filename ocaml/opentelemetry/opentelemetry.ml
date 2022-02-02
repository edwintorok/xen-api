module Proto = struct
  module Common = struct
    (** @canonical Opentelemetry.Proto.Common.V1 *)
    module V1 = Common.Opentelemetry.Proto.Common.V1
  end

module Resource = struct
    (** @canonical Opentelemetry.Proto.Resource.V1 *)
  module V1 = Resource.Opentelemetry.Proto.Resource.V1
end

module Collector = struct
  module Metrics = struct
    (** @canonical Opentelemetry.Proto.Collector.Metrics.V1 *)
    module V1 = Metrics_service.Opentelemetry.Proto.Collector.Metrics.V1
  end

  module Logs = struct
    (** @canonical Opentelemetry.Proto.Collector.Logs.V1 *)
    module V1 = Logs_service.Opentelemetry.Proto.Collector.Logs.V1
  end

  module Trace = struct
    (** @canonical Opentelemetry.Proto.Collector.Trace.V1 *)
    module V1 = Trace_service.Opentelemetry.Proto.Collector.Trace.V1
  end
end

module Metrics = struct
    (** @canonical Opentelemetry.Proto.Metrics.V1 *)
  module V1 = Metrics.Opentelemetry.Proto.Metrics.V1

    (** @canonical Opentelemetry.Proto.Metrics.Experimental *)
  module Experimental =
    Metrics_config_service.Opentelemetry.Proto.Metrics.Experimental
end

module Logs = struct
    (** @canonical Opentelemetry.Proto.Logs.V1 *)
  module V1 = Logs.Opentelemetry.Proto.Logs.V1
end

module Trace = struct
  module V1 = struct
    (** @canonical Opentelemetry.Proto.Trace.V1.TracesData *)
    module TracesData = Trace.Opentelemetry.Proto.Trace.V1.TracesData

    (** @canonical Opentelemetry.Proto.Trace.V1.ResourcesSpans *)
    module ResourceSpans = Trace.Opentelemetry.Proto.Trace.V1.ResourceSpans

    (** @canonical Opentelemetry.Proto.Trace.V1.Span *)
    module Span = Trace.Opentelemetry.Proto.Trace.V1.Span

    (** @canonical Opentelemetry.Proto.Trace.V1.Status *)
    module Status = Trace.Opentelemetry.Proto.Trace.V1.Status

    (** @canonical Opentelemetry.Proto.Trace.V1.TraceConfig *)
    module TraceConfig = Trace_config.Opentelemetry.Proto.Trace.V1.TraceConfig

    (** @canonical Opentelemetry.Proto.Trace.V1.ConstantSampler *)
    module ConstantSampler =
      Trace_config.Opentelemetry.Proto.Trace.V1.ConstantSampler

    (** @canonical Opentelemetry.Proto.Trace.V1.TraceIdRatioBased *)
    module TraceIdRatioBased =
      Trace_config.Opentelemetry.Proto.Trace.V1.TraceIdRatioBased

    (** @canonical Opentelemetry.Proto.Trace.V1.RateLimitingSampler *)
    module RateLimitingSampler =
      Trace_config.Opentelemetry.Proto.Trace.V1.RateLimitingSampler
  end
end

end
