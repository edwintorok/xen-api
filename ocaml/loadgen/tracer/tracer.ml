let config = Opentelemetry_client_ocurl.Config.make ~debug:true ()

(*
OTEL_EXPORTER_OTLP_ENDPOINT=10.71.57.164:4317'
 *)

let () =
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
