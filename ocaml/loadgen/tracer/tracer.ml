module Otel = Opentelemetry
let config = Opentelemetry_client_ocurl.Config.make ~debug:true ()

(*
OTEL_EXPORTER_OTLP_ENDPOINT=10.71.57.164:4317'
 *)

let last_begin = Hashtbl.create 47

let runtime_begin ring_id timestamp phase =
  Hashtbl.replace last_begin (ring_id, phase) timestamp

module Ze = Zero_http.Zero_events

let runtime_end ring_id timestamp phase =
  if Ze.Timestamp.initialized () then
  let start = Hashtbl.find last_begin (ring_id, phase) in
  let trace_id = (Opentelemetry.Scope.get_surrounding () |> Option.get).trace_id in
  let span, _ =
    Opentelemetry.Span.create
    ~trace_id
    ~start_time:(Ze.Timestamp.to_unix_nano start)
    ~end_time:(Ze.Timestamp.to_unix_nano timestamp)
    (Runtime_events.runtime_phase_name phase)
  in
  Opentelemetry.Trace.emit [ span ]
  

let () =
  Otel.Globals.service_name := "loadgen";
  (* don't set up Otel built-in GC metrics: we will read it using runtime events *)
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  let tmpdir = Filename.temp_dir "tracer" "event_dir" in
  let env =
    [|
       "OCAML_RUNTIME_EVENTS_START=1"
     ; "OCAML_RUNTIME_EVENTS_DIR=" ^ tmpdir
     ; "OCAML_RUNTIME_EVENTS_PRESERVE=1"
    |]
  in
  let finally () =
    (* TODO rm-rf dir *)
    Unix.rmdir tmpdir
  in
  Fun.protect ~finally @@ fun () ->
  let prog = Sys.argv.(1) in
  Otel.Trace.with_ "Run benchmark" @@ fun _scope ->
  Printf.printf "Launching %s\n" prog;
  let pid =
    Unix.create_process_env prog
      [|prog|]
      env Unix.stdin Unix.stdout Unix.stderr
  in
  Printf.printf "tmpdir: %s\n" tmpdir;
  (* FIXME: racy, wait for file creation *)
  Unix.sleepf 0.1;
  let cursor = Runtime_events.create_cursor @@ Some (tmpdir, pid) in
  let finally () =
     Runtime_events.free_cursor cursor;
     Sys.remove (Filename.concat tmpdir @@ Printf.sprintf "%d.events" pid);
  in
  Fun.protect ~finally @@ fun () ->
  let on_simple_span ~domain:_ ~timestamp_unix_ns:_ ~name:_ ~value:_ = (* TODO *) ()  in
  let callbacks =
    Runtime_events.Callbacks.create
      ~runtime_begin ~runtime_end
      (*~runtime_begin ~runtime_end ~runtime_counter ~alloc ~lifecycle ~lost_events *)
      ()
    |> Zero_http.Zero_events.register_callbacks ~on_simple_span
  in

  while fst (Unix.waitpid [Unix.WNOHANG] pid) = 0 do
    let (_ : int) = Runtime_events.read_poll cursor callbacks None in
    Unix.sleepf (1.0 /. 47.)
  done ;
  let (_ : int) = Runtime_events.read_poll cursor callbacks None in
  Unix.sleepf 2.; (* TODO: flush and wait, but cleanup call is wrong? *)
  ()
