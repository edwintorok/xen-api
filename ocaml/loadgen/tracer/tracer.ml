open Opentelemetry

let ns = Zero_http.Zero_events.Timestamp.to_int64

let delta = ref 0L

let timestamp t =
  let d = !delta in
  assert (not @@ Int64.equal d 0L);
  Int64.add !delta @@ ns t

let traceparent _ _ _ _ = ()

let http_request_url _ _ _ _ = ()

let http_request_body_size _ _ _ _ = ()

let request_id _ _ _ _ = ()

let connected _ _ _ _ = ()

let http_response_status_code _ _ _ _ = ()

let http_response_body_size _ _ _ _ = ()

let connecting _ _ _ _ = ()

let http_request_method _ _ _ _ = ()

let http_response_headers _ ts _ _ =
  timestamp ts |> Printf.printf "TODO: %Ld\n"

let http_response_body _ _ _ _ = ()

let rpc_system _ _ _ _ = ()

let last_timestamp = ref 0L


let nop _ timestamp _ _ = last_timestamp := ns timestamp


let realtime _ timestamp _ epoch_ns =
  Printf.printf "realtime: %.9f\n" (Int64.to_float epoch_ns /. 1e9);
  delta := Int64.sub epoch_ns (ns timestamp)

let () =
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
  let callbacks =
    Runtime_events.Callbacks.create
      (*~runtime_begin ~runtime_end ~runtime_counter ~alloc ~lifecycle ~lost_events *)
      ()
    |> Zero_http.Zero_events.register_callbacks ~traceparent ~http_request_url
         ~http_request_body_size ~request_id ~connected
         ~http_response_status_code ~http_response_body_size ~connecting
         ~http_request_method ~http_response_headers ~http_response_body
         ~rpc_system ~nop ~realtime
  in

  while fst (Unix.waitpid [Unix.WNOHANG] pid) = 0 do
    let (_ : int) = Runtime_events.read_poll cursor callbacks None in
    Unix.sleepf (1.0 /. 47.)
  done ;
  let (_ : int) = Runtime_events.read_poll cursor callbacks None in
  ()
