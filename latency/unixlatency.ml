let significant_figures = 3

let init () =
  Hdr_histogram.init ~lowest_discernible_value:1
    ~highest_trackable_value:1_000_000 ~significant_figures

(** doesn't allocate *)
let mtime () = Monotonic_clock.now () |> Int64.to_int

let rec measure_self hdr n t0 t1 =
  if n > 0 then (
    let ok = Hdr_histogram.record_value hdr (t1 - t0) in
    assert ok ;
    let t2 = mtime () in
    measure_self hdr (n - 1) t1 t2 )

let percentiles = [50.; 95.; 99.; 99.9; 100.]

let dump_stats hdr =
  Hdr_histogram.hdr_percentiles_print hdr "/dev/stdout" 5l 1e3 Hdr_histogram.CLASSIC

let measure_self m n =
  let hdr = init () in
  for _ = 1 to m do
    let t0 = mtime () in
    let t1 = mtime () in
    measure_self hdr n t0 t1 ;
    (* avoid being interrupted while measuring, go back to the OS from time to time *)
    Unix.sleepf 0.001
  done;
  dump_stats hdr ;
  Hdr_histogram.close hdr

let hdr = init ()

let measure socket addr () =
  let t0 = mtime () in
  Unix.connect socket addr ;
  let t1 = mtime () in
  let d = t1 - t0 in
  let ok = Hdr_histogram.record_value hdr d in
  assert ok

let run addr =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let finally () = Unix.close socket in
  Fun.protect ~finally (measure socket addr)

let print_header () =
  List.iter (Printf.printf "\t%4.0f%%") percentiles ;
  print_endline ""

let () =
    measure_self 1000 100 ;
  exit 0 ;
  let target = Sys.argv.(1) in
  let sockaddr = Unix.ADDR_UNIX target in
  print_header () ;
  for _ = 1 to 1000 do
    for _ = 1 to 20 do
      run sockaddr ; Unix.sleepf 0.047
    done ;
    dump_stats hdr
  done
