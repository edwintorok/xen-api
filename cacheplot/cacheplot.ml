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
  fun () -> Operations.StridedRead.read ~stride_size_bytes data

let[@inline always] start_workers (ready, _) =
  Workers.StartStop.start ready

let mfence = Ocaml_intrinsics.Fences.memory_fence

let test_strided_read ~linesize ~n =
  Test.make_indexed_with_resource ~name:(string_of_int n)
    ~args:(build_strides ~n ~stride_size_bytes:(linesize/2) [])
    Test.uniq
    ~allocate:(fun stride_size_bytes ->
      let r =
        Operations.StridedRead.allocate n,
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
    (fun stride_size_bytes ->
      Staged.stage (fun (data, _) ->
        Operations.StridedRead.read ~stride_size_bytes data)
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

let predictors = [|Measure.run|]
let instances = [ rdtsc ]

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:false ~bootstrap:0 ~predictors
  in
  let results =
    List.map (fun instance ->
      Analyze.all ols instance raw_results
    ) instances
  in
  Analyze.merge ols instances results

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
    results |> Hashtbl.iter @@ fun _ result ->
    result |> Hashtbl.iter @@ fun k' ols ->
    ols |> Analyze.OLS.estimates |> Option.iter @@ fun estimates ->
    estimates |> List.iter @@ fun est ->
    Scanf.sscanf k' "strided_read/%d:%d" @@ fun n stride ->
    let ops = n / stride in
    Hashtbl.replace tbl (stride, n) (est /. float ops)
  in
  let strides, columns = Hashtbl.fold (fun (stride, n) _ (strides, columns) ->
    IntSet.add stride strides, IntSet.add n columns
  ) tbl (IntSet.empty, IntSet.empty) in

  Printf.printf "%8s" "STRIDE";
  columns |> IntSet.iter (fun n ->
    print_char '\t';
    print_suffix n
  );
  print_char '\n';

  strides |> IntSet.iter @@ fun stride ->
  Printf.printf "%8d" stride;
  let () =
    columns |> IntSet.iter @@ fun n ->
    print_char '\t';
    Hashtbl.find_opt tbl (stride, n)
    |> Option.iter @@ fun result ->
    Printf.printf "%6.1f" result
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
  Arg.parse [
    "--random", Arg.Set random, "Use random rather than sequential reads"
  ; "--threads", Arg.Set_int threads,"Use specified number of threads"
  ; "--timeslice", Arg.Set_float timeslice, "Set thread yield timeslice"
  ] ignore "cacheplot [--random]";
  let cfg = Bechamel.Benchmark.cfg ~quota:(Time.second 0.1) ~start:2 ~stabilize:false () in
  let f = if !random then test_cycle_read else test_strided_read in
  if !timeslice > 0. then set_timeslice !timeslice;
  tests f |> Bechamel.Benchmark.all cfg instances |> analyze
  |> print_results
