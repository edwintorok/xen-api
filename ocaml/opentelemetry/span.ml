type t = {
    mutable raw: Proto.Trace.V1.Span.t
  ; mutable is_recording: bool
  ; sampled: bool
}

open Proto.Trace.V1.Span

let get_trace_id t = t.raw.trace_id

let get_span_id t = t.raw.span_id

let get_trace_state t = t.raw.trace_state
