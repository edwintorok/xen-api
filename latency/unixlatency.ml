let hdr = Hdr_histogram.init ~lowest_discernible_value:10 ~highest_trackable_value:(100_000_000_000) ~significant_figures:4

(** doesn't allocate *)
let mtime () = Monotonic_clock.now () |> Int64.to_int

let measure socket addr = fun () ->
  let t0 = mtime () in
  Unix.connect socket addr;
  let t1 = mtime() in
  let d = t1 - t0 in
  let ok = Hdr_histogram.record_value hdr d in
  assert ok

let run addr =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let finally () = Unix.close socket in
  Fun.protect ~finally (measure socket addr)

let percentiles = [50.; 95.; 99.; 100.]

let dump_stats () =
  let () =
    percentiles |> List.iter @@ fun p ->
    let v = (p |> Hdr_histogram.value_at_percentile hdr |> float) *. 1e-6 in
    Printf.printf "\t%.3fms" v
  in
  print_endline ""

let print_header () =
  List.iter (Printf.printf "\t%4.0f%%") percentiles;
  print_endline ""

let () =
  let target = Sys.argv.(1) in
  let sockaddr = Unix.ADDR_UNIX target in
  print_header ();
  for _ = 1 to 1000 do
    for _ = 1 to 20 do
      run sockaddr;
      Unix.sleepf 0.047
    done;
    dump_stats ()
  done
