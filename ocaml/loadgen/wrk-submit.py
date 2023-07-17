#!/usr/bin/python3
from opentelemetry import trace
from opentelemetry.sdk.trace import (TracerProvider, _Span)
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.exporter.jaeger.thrift import JaegerExporter
from opentelemetry.sdk.trace.export import (SimpleSpanProcessor, ConsoleSpanExporter, BatchSpanProcessor)
from opentelemetry.sdk.resources import SERVICE_NAME, Resource
from opentelemetry.sdk.trace.id_generator import RandomIdGenerator
from opentelemetry.trace.propagation.tracecontext import TraceContextTextMapPropagator

import csv

resource = Resource(attributes={SERVICE_NAME: "wrk"})

provider = TracerProvider(resource=resource)
#otlp_exporter = OTLPSpanExporter(endpoint='http://localhost:4317', insecure=True)


#otlp_exporter = OTLPSpanExporter(endpoint='http://10.71.57.164:4317', insecure=True)

jaeger_exporter = JaegerExporter(collector_endpoint="http://10.71.57.164:14268/api/traces?format=jaeger.thrift")
#processor = BatchSpanProcessor(otlp_exporter)

processor = BatchSpanProcessor(jaeger_exporter)
provider.add_span_processor(processor)

provider.add_span_processor(
    SimpleSpanProcessor(ConsoleSpanExporter())
)

#provider.add_span_processor(
#    BatchSpanProcessor(otlp_exporter)
#)

trace.set_tracer_provider(provider)

class ExistingIdGenerator(RandomIdGenerator):
    def __init__(self):
        super().__init__()
        self.custom_id = None

    def set_id(self, trace_id: int, span_id: int) -> None:
        self.custom_id = trace_id, span_id
        return self

    def __enter__(self):
        return self

    def __exit__(self, _exn_type, _exn_val, _exn_tb):
        self.custom_id = None

    def generate_span_id(self):
        return self.custom_id[1] if self.custom_id else super().generate_span_id()

    def generate_trace_id(self):
        return self.custom_id[0] if self.custom_id else super().generate_trace_id()

idgen = ExistingIdGenerator()

tracer = trace.get_tracer(__name__)
tracer.id_generator = idgen
process = None
thread = None
process_context = None
thread_context = None
process_end = None
timeout = int(30 * 1e6)

batch = 512 # TODO use higher
counter = 0

with open('timings.csv', 'r') as csvfile:
    reader = csv.DictReader(csvfile, delimiter=',')
    for row in reader:
        request_id = row['thread']

        # opentelementry wants an int as microseconds
        begin = int(float(row['start_time'])*1e9)
        end = row['finish_time']
        end = None if end == 'DNF' else int(float(end)*1e9)

        if request_id == 'PROCESS':
            _ver, trace_id, span_id, trace_flags = row['traceparent'].split('-')
            with idgen.set_id(int(trace_id, 16), int(span_id, 16)):
                process = tracer.start_span("Process", start_time = begin, kind = trace.SpanKind.CLIENT, attributes={"submit": 6})
                process.end(end_time = end)
                process_end = end
                process_context = trace.set_span_in_context(process)
        elif len(request_id) > 0:
                thread = tracer.start_span("Thread", start_time = begin, context = process_context, kind = trace.SpanKind.CLIENT)
                thread.end(end_time = end)
                thread_context = thread.get_span_context()
                thread_context2 = trace.set_span_in_context(thread, process_context)
        else:
            # textpropagator gives us a Context, and not a SpanContext
            _ver, trace_id, span_id, trace_flags = row['traceparent'].split('-')
            if end:
                status = trace.status.Status(trace.status.StatusCode.OK)
            else:
                end = min(process_end, begin+timeout)
                status = trace.status.Status(trace.status.StatusCode.ERROR, "Did not finish")
            span_attributes = row
            del span_attributes['start_time']
            del span_attributes['finish_time']
            del span_attributes['thread']
            del span_attributes['traceparent']

            with idgen.set_id(int(trace_id, 16), int(span_id, 16)):
                span = tracer.start_span("Request", kind = trace.SpanKind.CLIENT, start_time = begin, context = thread_context2, attributes = span_attributes)
                span.set_status(status)
                span.end(end_time = end)
        counter = counter + 1
        if counter >= batch:
            processor.force_flush()
            counter = 0

