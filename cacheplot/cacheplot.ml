open Simple_workloads
open Bechamel

module Rdtsc = struct
  type witness = unit
  let make = ignore
  let load = ignore
  let unload = ignore
  let label () = "RDTSC"
  let unit () = "cycle"

  let serialize =  Ocaml_intrinsics.Fences.load_fence

  let get () =
    serialize ();
    Ocaml_intrinsics.Perfmon.rdtsc () |> Int64.to_float
end

let rdtsc =
  let measure = Measure.register (module Rdtsc) in
  Measure.instance (module Rdtsc) measure

let rec build_strides ~n ~stride_size_bytes results =
  if stride_size_bytes > n / 2 then results
  else build_strides ~n ~stride_size_bytes:(stride_size_bytes * 2) (stride_size_bytes :: results)

let threads = ref 0

let worker ~stride_size_bytes n =
  (* always use strided read, faster *)
  let data = Operations.StridedRead.allocate n in
  let stride_size_words = stride_size_bytes / Operations.word_size in
  fun () -> Operations.StridedRead.read ~stride_size_words data

let[@inline always] start_workers (ready, _) =
  Workers.StartStop.start ready

let mfence = Ocaml_intrinsics.Fences.memory_fence

let test_strided_read ~linesize ~n =
  Test.make_indexed_with_resource ~name:(string_of_int n)
    ~args:(build_strides ~n ~stride_size_bytes:(linesize/2) [])
    Test.uniq
    ~allocate:(fun stride_size_bytes ->
      let data, threads =
        Operations.StridedRead.allocate n,
        let t = Workers.allocate (worker ~stride_size_bytes n) !threads in
        start_workers t;
        t
      in
      let stride_size_words = stride_size_bytes / Operations.word_size in
      (* warmup, otherwise vmin/vmax will be very different to the estimate *)
      Sys.opaque_identity (Operations.StridedRead.read ~stride_size_words data);
      mfence ();
      data, threads
    )
    ~free:(fun (_, threads) ->
      Workers.free threads;
      output_char stderr '.'; flush stderr;
      mfence ()
    )
    (fun stride_size_bytes ->
      let stride_size_words = stride_size_bytes / Operations.word_size in
      Staged.stage (fun (data, _) ->
         Operations.StridedRead.read ~stride_size_words data
      )
    )

let test_cycle_read ~linesize:_ ~n =
  Test.make_indexed_with_resource ~name:(string_of_int n)
    ~args:(build_strides ~n ~stride_size_bytes:Operations.word_size [])
    Test.uniq
    ~allocate:(fun stride_size_bytes ->
      let r =
       Operations.CycleRead.allocate ~stride_size_bytes n,
       let t = Workers.allocate (worker ~stride_size_bytes n) !threads in
       start_workers t;
       t
      in
      mfence ();
      r
    )
    ~free:(fun (_, threads) ->
      Workers.free threads;
      output_char stderr '.'; flush stderr;
      mfence ()
    )
    (fun _ -> Staged.stage (fun (data, _) ->
      Operations.CycleRead.read data))

let rec make_tests f ~linesize ~lo ~hi tests =
  if lo > hi then tests
  else begin
    let test = f ~linesize ~n:lo in
    make_tests f ~linesize ~lo:(lo*2) ~hi (test :: tests)
  end
  
let caches = Cachesize.caches ()
let linesize = List.fold_left Int.max 0 (List.map (fun c -> c.Cachesize.linesize) caches)

let tests f =
  let hi = (List.hd caches).Cachesize.size * 2
  and lo = (caches |> List.rev |> List.hd).Cachesize.size / 2 in
  make_tests f ~lo ~hi ~linesize []
  |> Bechamel.Test.make_grouped ~name:"strided_read"

let predictor = Measure.run
let instance = rdtsc

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:false ~predictors:[|predictor|] ~bootstrap:0
  in
  let label = Measure.label instance in
  raw_results |> Hashtbl.to_seq |> Seq.map @@ fun (name, result) ->
  let vmin, vmax = result.Benchmark.lr |> Array.fold_left (fun (vmin,vmax) m ->
    let v = Measurement_raw.get ~label m /. Measurement_raw.run m in
    Float.min vmin v, Float.max vmax v
  ) (Float.max_float, Float.min_float) in
  name, Analyze.one ols instance result, vmin, vmax

let print_suffix n =
  if n < 1 lsl 10 then
    Printf.printf "%6d" n
  else if n < 1 lsl 20 then
    Printf.printf "%3dKiB" (n lsr 10)
  else Printf.printf "%3dMiB" (n lsr 20)

module IntSet = Set.Make(Int)

let print_results results =
  let tbl = Hashtbl.create 47 in
  let () =
    results |> Seq.iter @@ fun (name, ols, vmin, vmax) ->
    ols |> Analyze.OLS.estimates |> Option.iter @@ fun estimates ->
    estimates |> List.iter @@ fun est ->
    Scanf.sscanf name "strided_read/%d:%d" @@ fun n stride ->
    let ops = float (n / stride) in
    Hashtbl.replace tbl (stride, n) (est /. ops, vmin /. ops, vmax /. ops)
  in
  let strides, columns = Hashtbl.fold (fun (stride, n) _ (strides, columns) ->
    IntSet.add stride strides, IntSet.add n columns
  ) tbl (IntSet.empty, IntSet.empty) in

  Printf.printf "%8s" "STRIDE";
  columns |> IntSet.iter (fun n ->
    print_char '\t';
    print_suffix n;
    Printf.printf "\t%6s\t%6s" "min" "max"
  );
  print_char '\n';

  strides |> IntSet.iter @@ fun stride ->
  Printf.printf "%8d" stride;
  let () =
    columns |> IntSet.iter @@ fun n ->
    print_char '\t';
    match Hashtbl.find_opt tbl (stride, n) with
    | Some (result, vmin, vmax) ->
      Printf.printf "%6.1f\t%6.1f\t%6.1f" result vmin vmax
    | None -> print_char '\t'; print_char '\t'
  in
  print_char '\n'

let handler (_:int) =
  Thread.yield ()

let set_timeslice slice =
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle handler);
  let (_:Unix.interval_timer_status) = Unix.setitimer Unix.ITIMER_VIRTUAL Unix.{it_value = slice; it_interval = slice} in
  ()

let () =
  let random = ref false in
  let timeslice = ref 0. in
  let bench_time = ref 0.1 in
  Arg.parse [
    "--random", Arg.Set random, "Use random rather than sequential reads"
  ; "--threads", Arg.Set_int threads,"Use specified number of threads"
  ; "--timeslice", Arg.Set_float timeslice, "Set thread yield timeslice"
  ; "--benchtime", Arg.Set_float bench_time, "Benchmark minimum time per cell"
  ] ignore "cacheplot [--random]";
  (* to allow equal number of memory reads to execute Strided_read.read with stride=N/2,
     and with stride=linesize we need an iteration limit of at least:
     limit = N/linesize/2
     and we might want to repeat this multiple times for accuracy
   *)
  let limit = 100 * (List.hd caches).Cachesize.size / linesize / 2 in
  let cfg = Bechamel.Benchmark.cfg ~limit ~quota:(Time.second !bench_time) ~start:2 ~stabilize:false () in
  let f = if !random then test_cycle_read else test_strided_read in
  if !timeslice > 0. then set_timeslice !timeslice;
  tests f |> Bechamel.Benchmark.all cfg [instance] |> analyze
  |> print_results
