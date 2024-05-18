let init () =
  Hdr_histogram.init ~lowest_discernible_value:32
    ~highest_trackable_value:500_000_000_000 ~significant_figures:3

(** doesn't allocate *)
let mtime () = Monotonic_clock.now () |> Int64.to_int

let measure hdr f arg1 =
  let t0 = mtime () in
  let r = f arg1 in
  let t1 = mtime () in
  let ok = Hdr_histogram.record_value hdr (t1 - t0) in
  assert ok;
  r

let dump_stats name hdr =
  Hdr_histogram.hdr_percentiles_print hdr name 5l 1e3 Hdr_histogram.CLASSIC;
  Printf.eprintf "Wrote %s\n%!" name

let measure_self n =
  let hdr = init ()
  and hdr' = init () in
  for i = 1 to 1000 do
    let ok = measure hdr (Hdr_histogram.record_value hdr') i in
    assert ok
  done;
  dump_stats "self.histogram" hdr;
  Hdr_histogram.close hdr;
  Hdr_histogram.close hdr'

let connect_hdr = init ()
let sleep_hdr = init ()

let run addr =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let finally () = Unix.close socket in
  Fun.protect ~finally
  (fun () -> measure connect_hdr (Unix.connect socket) addr)

let () =
  measure_self 100 ;
  let target = Sys.argv.(1) in
  let sockaddr = Unix.ADDR_UNIX target in
  
  (* todo: calc next sleep time, could be 0 if overrun, but count from actual schedule *)
  Sys.catch_break true;
  let finally () =
    dump_stats "connect.histogram" connect_hdr;
    dump_stats "sleepf.histogram" sleep_hdr
  in

  (* TODO: connect, send OPTIONS / , Connection:close,
     read all

     Measure latency for: first packet, entire response (read until EOF)

     Also print 'testing' and a '.' after each attempt (fflush)

     have a mode where we only send 1 char (cmdline flag), used to test virtual itimer effectiveness
     on 100+16 test.
  *)
  
  let run () = 
    for _ = 1 to 100 do
      for _ = 1 to 20 do
        run sockaddr ;
        Gc.minor ();
        measure sleep_hdr Unix.sleepf 0.0047
      done ;
    done
  in
  Gc.full_major ();
  Fun.protect ~finally run
