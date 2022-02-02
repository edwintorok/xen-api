(* ocaml-protoc-plugin wraps each .proto file into OpenTelemetry.Proto...,
   and we can't easily merge these by just including the toplevel file since it'd just keep
   overwriting the OpenTelemetry module *)
module Proto = struct
  module Common = struct
    module V1 = Common.Opentelemetry.Proto.Common.V1
  end
  module Resource = struct
    module V1 = Resource.Opentelemetry.Proto.Resource.V1
  end
  module Collector = struct
    module Metrics = struct
      module V1 = Metrics_service.Opentelemetry.Proto.Collector.Metrics.V1
    end
    module Logs = struct
      module V1 = Logs_service.Opentelemetry.Proto.Collector.Logs.V1
    end
    module Trace = struct
      module V1 = Trace_service.Opentelemetry.Proto.Collector.Trace.V1
    end
  end
  module Metrics = struct
    module V1 = Metrics.Opentelemetry.Proto.Metrics.V1
    module Experimental = Metrics_config_service.Opentelemetry.Proto.Metrics.Experimental
  end
  module Logs = struct
    module V1 = Logs.Opentelemetry.Proto.Logs.V1
  end
  module Trace = struct
    module V1 = struct
      include Trace_config.Opentelemetry.Proto.Trace.V1
      include Trace.Opentelemetry.Proto.Trace.V1
    end
  end
end
