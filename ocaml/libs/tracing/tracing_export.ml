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

module D = Debug.Make (struct let name = "tracing_export" end)

module Delay = Xapi_stdext_threads.Threadext.Delay
open D
open Tracing
module Otel = Opentelemetry

let ( let@ ) f x = f x

let export_interval = ref 30.

let set_export_interval t = export_interval := t

let host_id = ref "localhost"

let set_host_id id = host_id := id

let service_name = ref "unknown"

let set_service_name name =
  Otel.Globals.service_name := name ;
  service_name := name

let get_service_name () = !service_name

module Content = struct
  module Json = struct
    module ZipkinV2 = struct
      (* Module that helps export spans under Zipkin protocol, version 2. *)
      module ZipkinSpan = struct
        type zipkinEndpoint = {serviceName: string} [@@deriving rpcty]

        type annotation = {timestamp: int; value: string} [@@deriving rpcty]

        type t = {
            id: string
          ; traceId: string
          ; parentId: string option
          ; name: string
          ; timestamp: int
          ; duration: int
          ; kind: string option
          ; localEndpoint: zipkinEndpoint
          ; annotations: annotation list
          ; tags: (string * string) list
        }
        [@@deriving rpcty]

        type t_list = t list [@@deriving rpcty]

        let kind_to_zipkin_kind = function
          | SpanKind.Internal ->
              None
          | k ->
              Some k

        let json_of_t_list s =
          Rpcmarshal.marshal t_list.Rpc.Types.ty s |> Jsonrpc.to_string
      end

      let zipkin_span_of_span (s : Span.t) : ZipkinSpan.t =
        let serviceName = get_service_name () in
        let annotations =
          s
          |> Span.get_events
          |> List.map (fun event : ZipkinSpan.annotation ->
                 let timestamp =
                   int_of_float (event.SpanEvent.time *. 1000000.)
                 in
                 let value = event.SpanEvent.name in
                 {timestamp; value}
             )
        in
        {
          id= s |> Span.get_context |> SpanContext.span_id_of_span_context
        ; traceId= s |> Span.get_context |> SpanContext.trace_id_of_span_context
        ; parentId=
            s
            |> Span.get_parent
            |> Option.map (fun x ->
                   x |> Span.get_context |> SpanContext.span_id_of_span_context
               )
        ; name= s |> Span.get_name
        ; timestamp= int_of_float (Span.get_begin_time s *. 1000000.)
        ; duration=
            Option.value (Span.get_end_time s)
              ~default:(Unix.gettimeofday () *. 1000000.)
            -. Span.get_begin_time s
            |> ( *. ) 1000000.
            |> int_of_float
        ; kind=
            s
            |> Span.get_span_kind
            |> ZipkinSpan.kind_to_zipkin_kind
            |> Option.map SpanKind.to_string
        ; localEndpoint= {serviceName}
        ; annotations
        ; tags= Span.get_attributes s
        }

      let content_of (spans : Span.t list) =
        List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
    end
  end
end

module Destination = struct
  module File = struct
    let trace_log_dir = ref "/var/log/dt/zipkinv2/json"

    let max_file_size = ref (1 lsl 20)

    let compress_tracing_files = ref true

    let set_trace_log_dir dir = trace_log_dir := dir

    let get_trace_log_dir () = !trace_log_dir

    let set_max_file_size size = max_file_size := size

    let set_compress_tracing_files enabled = compress_tracing_files := enabled

    let file_name = ref None

    let lock = Mutex.create ()

    let make_file_name ?(ext = ".ndjson") () =
      let date = Ptime_clock.now () |> Ptime.to_rfc3339 ~frac_s:6 in
      let ( // ) = Filename.concat in
      let name =
        !trace_log_dir
        // String.concat "-" [get_service_name (); !host_id; date]
        ^ ext
      in
      file_name := Some name ;
      name

    let with_fd file_name =
      Xapi_stdext_unix.Unixext.with_file file_name
        [O_WRONLY; O_CREAT; O_APPEND]
        0o700

    let write fd str =
      let content = str ^ "\n" in
      ignore @@ Unix.write_substring fd content 0 (String.length content)

    let export_to ?ext writer =
      try
        let file_name =
          match !file_name with None -> make_file_name ?ext () | Some x -> x
        in
        Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file_name) 0o700 ;
        let@ fd = file_name |> with_fd in
        writer fd ;
        if (Unix.fstat fd).st_size >= !max_file_size then (
          debug "Tracing: Rotating file %s > %d" file_name !max_file_size ;
          if !compress_tracing_files then
            Zstd.Fast.compress_file Zstd.Fast.compress ~file_path:file_name
              ~file_ext:"zst" ;
          ignore @@ make_file_name ?ext ()
        ) ;
        Ok ()
      with e -> Error e

    let export json = export_to (fun fd -> write fd json)

    let with_raw_stream f =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () -> f export_to)

    let with_stream f =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () -> f export)
  end

  module Http = struct
    module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
    module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

    let export ?(verbose = false) ?(headers = [])
        ?(content_type = "application/json") ~url () body =
      try
        let content_headers =
          [
            ("Content-Type", content_type)
          ; ("Content-Length", string_of_int (String.length body))
          ]
        in
        let host =
          match (Uri.host url, Uri.port url) with
          | None, _ ->
              None
          | Some host, None ->
              Some host
          | Some host, Some port ->
              Some (Printf.sprintf "%s:%d" host port)
        in
        let host_headers =
          Option.fold ~none:[] ~some:(fun h -> [("Host", h)]) host
        in
        let split_header s =
          match Astring.String.cut ~sep:":" s with
          | Some (k, v) ->
              (k, String.trim v)
          | None ->
              (s, "")
        in
        let headers =
          List.concat
            [headers |> List.map split_header; content_headers; host_headers]
          |> Cohttp.Header.of_list
        in

        Open_uri.with_open_uri url (fun fd ->
            let request =
              Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers url
            in
            if verbose then
              Format.eprintf "> %a@." Cohttp.Request.pp_hum request ;
            (* `with_open_uri` already closes the `fd`. And therefore
               according to the documentation of `in_channel_of_descr` and
               `out_channel_of_descr` we should not close the channels on top of
               `fd`. *)
            let ic = Unix.in_channel_of_descr fd in
            let oc = Unix.out_channel_of_descr fd in
            Request.write
              (fun writer -> Request.write_body writer body)
              request oc ;
            (* We flush instead of closing the sending stream as nginx responds to a TCP
               half-shutdown with a full shutdown of both directions of the HTTP request *)
            flush oc ;
            match try Response.read ic with _ -> `Eof with
            | `Eof ->
                if verbose then
                  prerr_endline "EOF";
                Ok ()
            | `Invalid x ->
                if verbose then
                  Format.eprintf "INVALID: %s@." x;
                Error (Failure ("invalid read: " ^ x))
            | `Ok response
              when Cohttp.Code.(response.status |> code_of_status |> is_error)
              ->
                if verbose then
                  Format.eprintf "> %a@." Cohttp.Response.pp_hum response;
                Error (Failure (Cohttp.Code.string_of_status response.status))
            | `Ok response ->
                if verbose then
                  Format.eprintf "> %a@." Cohttp.Response.pp_hum response;
                Ok ()
        )
      with e -> Error e
  end

  module FileCollector = struct
    open Otel.Proto
    open Otel.Collector

    (* batching can be done in a batching collector *)

    (* OCaml's runtime already has an internal lock around channel operations,
       however for efficient Pbrt.Encoder.write_chunks (avoiding to_string),
       we need a lock around the encoder
    *)

    let with_encoder =
      (* protected by the mutex in File.with_raw_stream *)
      let encoder = Pbrt.Encoder.create () in
      fun f ->
        let (_ : (_, _) result) =
          let@ export = File.with_raw_stream in
          Pbrt.Encoder.reset encoder ;
          f encoder ;
          let@ fd = export ~ext:".otel" in
          let write buf pos len =
            let (_ : int) = Unix.write fd buf pos len in
            ()
          in
          Pbrt.Encoder.write_chunks write encoder
        in
        ()

    let send_logs : Logs.resource_logs list sender =
      {
        send=
          (fun resource_logs ~ret ->
            let m =
              Logs_service.default_export_logs_service_request ~resource_logs ()
            in
            let () =
              let@ encoder = with_encoder in
              Logs_service.encode_pb_export_logs_service_request m encoder
            in
            ret ()
          )
      }

    let send_metrics : Metrics.resource_metrics list sender =
      {
        send=
          (fun resource_metrics ~ret ->
            let m =
              Metrics_service.default_export_metrics_service_request
                ~resource_metrics ()
            in
            let () =
              let@ encoder = with_encoder in
              Metrics_service.encode_pb_export_metrics_service_request m encoder
            in
            ret ()
          )
      }

    let send_trace : Trace.resource_spans list sender =
      {
        send=
          (fun resource_spans ~ret ->
            let m =
              Trace_service.default_export_trace_service_request ~resource_spans
                ()
            in
            let () =
              let@ encoder = with_encoder in
              Trace_service.encode_pb_export_trace_service_request m encoder
            in
            ret ()
          )
      }

    let signal_emit_gc_metrics () = () (* not implemented *)

    let on_tick = Atomic.make (Otel.AList.make ())

    let tick () =
      on_tick |> Atomic.get |> Otel.AList.get |> List.iter (fun f -> f ())

    let set_on_tick_callbacks lst = Atomic.set on_tick lst

    let cleanup () = ()
  end

  (* TODO: BatchingCollector *)

  let export_to_endpoint parent traces endpoint =
    debug "Tracing: About to export" ;
    try
      File.with_stream (fun file_export ->
          let export, name =
            match endpoint with
            | Url url ->
                (Http.export ~url (), "Tracing.Http.export")
            | Bugtool ->
                (file_export, "Tracing.File.export")
          in
          let all_spans =
            Hashtbl.fold (fun _ spans acc -> spans @ acc) traces []
          in
          let attributes =
            [
              ("export.span.count", all_spans |> List.length |> string_of_int)
            ; ("export.endpoint", endpoint_to_string endpoint)
            ; ( "xs.tracing.spans_table.count"
              , Spans.span_count () |> string_of_int
              )
            ; ( "xs.tracing.finished_spans_table.count"
              , traces |> Hashtbl.length |> string_of_int
              )
            ]
          in
          let@ _ = with_tracing ~parent ~attributes ~name in
          all_spans
          |> Content.Json.ZipkinV2.content_of
          |> export
          |> Result.iter_error raise
      )
    with exn ->
      debug "Tracing: unable to export span : %s" (Printexc.to_string exn)

  let flush_spans () =
    let span_list = Spans.since () in
    let attributes =
      [("export.traces.count", Hashtbl.length span_list |> string_of_int)]
    in
    let@ parent =
      with_tracing ~parent:None ~attributes ~name:"Tracing.flush_spans"
    in
    TracerProvider.get_tracer_providers ()
    |> List.filter TracerProvider.get_enabled
    |> List.concat_map TracerProvider.get_endpoints
    |> List.iter (export_to_endpoint parent span_list)

  let delay = Delay.make ()

  (* Note this signal will flush the spans and terminate the exporter thread *)
  let signal () = Delay.signal delay

  let create_exporter () =
    enable_span_garbage_collector () ;
    (* TODO: MUST have separate flag, because this exports a lot of internal logs *)
    Otel.Collector.set_backend (module FileCollector) ;
    Thread.create
      (fun () ->
        let signaled = ref false in
        while not !signaled do
          debug "Tracing: Waiting %d seconds before exporting spans"
            (int_of_float !export_interval) ;
          if not (Delay.wait delay !export_interval) then (
            debug "Tracing: we are signaled, export spans now and exit" ;
            signaled := true
          ) ;
          flush_spans ()
        done
      )
      ()

  let exporter = ref None

  let lock = Mutex.create ()

  let main () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match !exporter with
        | None ->
            let tid = create_exporter () in
            exporter := Some tid ;
            tid
        | Some tid ->
            tid
    )
end

let flush_and_exit = Destination.signal

let main = Destination.main
