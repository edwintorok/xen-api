(*
* Copyright (C) 2024 Cloud Software Group
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; version 2.1 only. with the special
* exception on linking described in file LICENSE.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*)

module Otel = Opentelemetry

(* -- old code --- *)

(* The context of a trace that can be propagated across service boundaries. *)
module TraceContext = struct
    type traceparent = string [@@deriving yojson]
 
   type baggage = (string * string) list [@@deriving yojson]

   type t = {traceparent: traceparent option; baggage: baggage option}
   [@@deriving yojson]

   let empty = {traceparent= None; baggage= None}
   let with_traceparent traceparent ctx = {ctx with traceparent}

   let with_baggage baggage ctx = {ctx with baggage}

   let traceparent_of ctx = ctx.traceparent

   let baggage_of ctx = ctx.baggage

   let to_json_string t = Yojson.Safe.to_string (to_yojson t)

   let of_json_string s = of_yojson (Yojson.Safe.from_string s)
end

(* -- new code --- *)

(* Ambient-context only needs to be initialized if we want to use something other than TLS,
   e.g. the Lwt one *)

module SpanContext = struct
  type t =
    { span_ctx: Otel.Span_ctx.t 
    ; trace_context: TraceContext.t }

  let to_traceparent t =
    (* the [bytes] is created inside this call, so we are allowed to transform it back to a string
       without copying (see docstring of [Bytes.unsafe_to_string]) *)
    t.span_ctx |> Otel.Span_ctx.to_w3c_trace_context |> Bytes.unsafe_to_string

  let of_traceparent traceparent =
    match  Otel.Trace_context.Traceparent.of_value traceparent with
    | Error _ -> None
    | Ok (trace_id, parent_id) ->
      let span_ctx = Otel.Span_ctx.make ~trace_id ~parent_id ()
      and trace_context = TraceContext.(with_traceparent (Some traceparent) empty) in
      Some { span_ctx; trace_context }

  let of_trace_context ctx =
    ctx |> TraceContext.traceparent_of |> Option.map of_traceparent |> Option.join
  
  let trace_id_of_span_context t = Otel.Span_ctx.trace_id t.span_ctx
  
  let span_id_of_span_context t = Otel.Span_ctx.parent_id t.span_ctx
  
  let context_of_span_context t = t.trace_context

end

module Span = struct
 (* a scope is mutable, a span isn't, so collect everything into a Scope and emit at the end *)
 type t = Otel.Scope.t

 let get_context = Otel.Scope.to_span_ctx

end
