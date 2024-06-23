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


let rec retry_eintr f a b c d =
  try f a b c d
  with Unix.Unix_error(Unix.EINTR, _, _) ->
    retry_eintr f a b c d
  

let instance_of_atomic ~name ~unit atomic =
  let module M = struct
    type witness = int Atomic.t

    let name = name
   let label _ = name
    let unit _ = unit
    let make () = atomic
    let load = ignore
    let unload = ignore
    let get t =
      Atomic.get t |> float_of_int
  end
  in
  let measure = Measure.register (module M) in
  Measure.instance (module M) measure

let yields = instance_of_atomic ~name:"yields" ~unit:"yield" Operations.Yield.count

let read_operations = Atomic.make 0

let reads = instance_of_atomic ~name:"operations" ~unit:"read" read_operations

module type READ = sig
  type t
  val allocate : stride_size_bytes:int -> int -> t
  val read : stride_size_words:int -> t -> unit
end

module Make(R: READ) = struct
  type t = R.t * int
  let allocate ~stride_size_bytes n =
    let operations = n / stride_size_bytes in
    R.allocate ~stride_size_bytes n, operations
  
  let[@inline] read ~stride_size_words (data, operations) =
    let r = Sys.opaque_identity (R.read ~stride_size_words data) in
    let (_:int) = Atomic.fetch_and_add read_operations operations in    
    r
end

module StridedRead = Make(struct
  type t = int array
  let allocate ~stride_size_bytes:_ n = Operations.StridedRead.allocate n
  let[@inline] read ~stride_size_words data = Operations.StridedRead.read ~stride_size_words data
end)

module CycleRead = Make(struct
  type t = int array
  let allocate = Operations.CycleRead.allocate
  let[@inline] read ~stride_size_words:_ data = Operations.CycleRead.read data
end)

let[@inline] start_workers (ready, _) =
  Workers.StartStop.start ready

let mfence = Ocaml_intrinsics.Fences.memory_fence

let short = ref false

let worker ~stride_size_bytes n =
  (* always use strided read, faster *)
  let data = StridedRead.allocate ~stride_size_bytes n in
  let stride_size_words = stride_size_bytes / Operations.word_size in
  fun () ->
  StridedRead.read ~stride_size_words data

let rec build_strides ~n ~stride_size_bytes results =
  if stride_size_bytes > n / 2 then results
  else build_strides ~n ~stride_size_bytes:(stride_size_bytes * 2) (stride_size_bytes :: results)

let threads = ref 0

let process = ref false

let handler (_:int) =
  Operations.Yield.perform ()

let set_timeslice slice =
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle handler);
  let signal, kind = if (slice < 0.001) then Sys.sigalrm, Unix.ITIMER_REAL else Sys.sigvtalrm, Unix.ITIMER_VIRTUAL in
  Sys.set_signal signal (Sys.Signal_handle handler);
  let (_:Unix.interval_timer_status) = Unix.setitimer kind Unix.{it_value = slice; it_interval = slice} in
  ()

let short_stride_size = ref 0

let[@inline] test_read (module R: READ) ~timeslice ~linesize ~hi ~n =
  let min_stride_size = linesize in
  let args =
    if !short then [Int.max !short_stride_size linesize(*; 4096*)]
    else build_strides ~n ~stride_size_bytes:min_stride_size []
  in
  let loops ~stride_size_bytes =
      (* do same number of reads overall, even with large strides, or small arrays,
         this tries to ensure we get interrupted similar number of times
       *)
      (stride_size_bytes / min_stride_size) * (hi / n)
  in
  let allocate stride_size_bytes =
      let data, threads =
        R.allocate ~stride_size_bytes n,
        let t = Workers.allocate (worker ~stride_size_bytes n) !threads in
        start_workers t;
        t
      in
      mfence ();
      data, threads, loops ~stride_size_bytes
  and free (_, threads, _) =
      Workers.free threads;
      output_char stderr '.'; flush stderr;
      mfence ()
  and run stride_size_bytes =
      let stride_size_words = stride_size_bytes / Operations.word_size in
      Staged.stage (fun (data, _, loops) ->
         for _ = 1 to loops do
           Sys.opaque_identity (R.read ~stride_size_words data)
         done
      )
  in
  let allocate_process stride_size_bytes =
    let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let pid = Unix.fork () in
    if pid = 0 then begin
      (* child *)
      if timeslice > 0. then set_timeslice timeslice;
      Unix.close s1;
      let b = Bytes.create 1 in
      let run = run stride_size_bytes in
      let data = allocate stride_size_bytes in
      while retry_eintr Unix.read s2 b 0 1 > 0 do
        let () = Sys.opaque_identity (Staged.unstage run data) in
        let n = Unix.write_substring s2 "y" 0 1 in
        assert (n = 1)
      done;
      (* we could free it, but we are exiting anyway *)
      exit 1
    end
    else begin
      Unix.close s2;
      let operations = n / stride_size_bytes in
      pid, s1, Bytes.create 1, loops ~stride_size_bytes * operations
    end
  and free_process (pid, s1, _, _) =
    Unix.close s1;
    Unix.kill pid Sys.sigkill;
    output_char stderr '.'; flush stderr
  and run_process (_stride_size_bytes, s1, buf, operations) =
    let n = Unix.write_substring s1 "x" 0 1 in
    assert (n = 1);
    let n = retry_eintr Unix.read s1 buf 0 1 in
    assert (n = 1);
    Atomic.fetch_and_add read_operations operations
  in 
  if !process then
    Test.make_indexed_with_resource ~name:(string_of_int n)
      ~args
      Test.uniq
      ~allocate:allocate_process
      ~free:free_process
      (fun _ -> Staged.stage run_process)
  else
    Test.make_indexed_with_resource ~name:(string_of_int n)
      ~args
      Test.uniq
      ~allocate
      ~free
      run


let test_strided_read ~linesize ~hi ~n = test_read (module StridedRead) ~linesize ~hi ~n
let test_cycle_read ~linesize ~hi ~n = test_read (module CycleRead) ~linesize ~hi ~n

let rec make_tests f ~timeslice ~linesize ~lo ~hi tests =
  if lo > hi then tests
  else begin
    let test = f ~linesize ~hi ~n:lo ~timeslice in
    make_tests f ~timeslice ~linesize ~lo:(lo*2) ~hi (test :: tests)
  end
  
let caches = Cachesize.caches ()
let linesize = List.fold_left Int.max 0 (List.map (fun c -> c.Cachesize.linesize) caches)

let tests timeslice f =
  let tests =
    let lo = (caches |> List.rev |> List.hd).Cachesize.size / 2 in
    if !short then
      (* TODO: this could be factored out if we constructed the list of sizes first in both cases *)
      let hi = (List.hd caches).Cachesize.size * 3/4 in
      (* TODO: for larger stride size we may want to allocate a larger array, to ensure these properties *)
      (* fits L1: L1*0.75,
         fits L2: L2*0.75,
         fits L3: L3*0.75
         to reduce measurement time we don't include L3*2, which measure main memory access times
         we use 0.75*L to avoid the last few dropping out due to extra code/overhead,
            e.g. when handling the socket
      *)
      lo :: List.rev_map (fun c -> c.Cachesize.size * 3/4) caches
      |> List.map (fun n -> f ~linesize ~hi ~n ~timeslice)
    else
      let hi = (List.hd caches).Cachesize.size in
      make_tests f ~timeslice ~lo ~hi ~linesize []
  in
  Bechamel.Test.make_grouped ~name:"strided_read" tests


let predictor = Measure.label reads
let instance = rdtsc
let instances = [instance; reads]

let bootstrap = ref false

let analyze raw_results =
  let bootstrap = if !bootstrap then 3000 else 0 in
  let ols =
    (* don't use yields here as predictor (even though it results in better R^2), it results in a very underestimated rdtsc/run, lower than min measured! *)
    Analyze.ols ~r_square:true ~predictors:[|predictor|]
  in
  ols ~bootstrap, raw_results,
  (* TODO: we probably only need to run OLS on the instance we care about *)
  instances |> List.map @@ fun instance ->
  let label = Measure.label instance in
  raw_results |> Hashtbl.to_seq |> Seq.filter_map @@ fun (name, result) ->
  let vmin, vmax = result.Benchmark.lr |> Array.fold_left (fun (vmin,vmax) m ->
    let v = Measurement_raw.get ~label m /. Measurement_raw.get ~label:predictor m in
    Float.min vmin v, Float.max vmax v
  ) (Float.max_float, 0.) in
  try
    let r = Analyze.one (ols ~bootstrap) instance result in
    (*Format.eprintf "%a@." Analyze.OLS.pp r;*)
    let vmin, vmax = match Analyze.OLS.ci95 r with
      | Some [ci95] -> let open Analyze.OLS.Ci95 in ci95.l, ci95.r
      | _ -> vmin, vmax
    in
    Some (name, r, (vmin, vmax))
  with _ ->
    (* if bootstrap fails, we drop *)
    let r = Analyze.one (ols ~bootstrap:0) instance result in
    Format.eprintf "Dropping (bootstrap failure) %s, %a@," name Analyze.OLS.pp r;
    None

let analyze' ols raw_results results =
  results |> List.map (fun x -> x |> Seq.map (fun (name, ols, _) ->
   name, ols)
  |> Hashtbl.of_seq)
  |> Analyze.merge ols instances, raw_results

let print_suffix n =
  if n < 1 lsl 10 then
    Printf.printf "%6d" n
  else if n < 1 lsl 20 then
    Printf.printf "%3dKiB" (n lsr 10)
  else Printf.printf "%3dMiB" (n lsr 20)

module IntSet = Set.Make(Int)

let print_results ?(print_header=true) timeslice results =
  let tbl = Hashtbl.create 47 in
  let () =
    results |> List.hd |> Seq.iter @@ fun (name, ols, (vmin, vmax)) ->
    Format.eprintf "%a@," Analyze.OLS.pp ols;
    let r_square = Analyze.OLS.r_square ols in
    if r_square = None || Option.get r_square >= 0.2 then
      (* skip bad points *)
      ols |> Analyze.OLS.estimates |> Option.iter @@ fun estimates ->
      estimates |> List.iter @@ fun est ->
      Scanf.sscanf name "strided_read/%d:%d" @@ fun n stride ->
      Hashtbl.replace tbl (stride, n) (est, vmin, vmax)
    else
      (* TODO: this means we won't print some headers if we drop all measurements for that one on first iteration *)
      Format.eprintf "dropping %s: %a@," name Analyze.OLS.pp ols
  in
  let strides, columns = Hashtbl.fold (fun (stride, n) _ (strides, columns) ->
    IntSet.add stride strides, IntSet.add n columns
  ) tbl (IntSet.empty, IntSet.empty) in
  if print_header then begin
    output_char stderr '\n';
    flush stderr;
    Printf.printf "%8s\t%8s\t%8s" "THREADS" "TIMESLICE" "STRIDE";
    columns |> IntSet.iter (fun n ->
      print_char '\t';
      print_suffix n;
      Printf.printf "\t%6s" "min";
      Printf.printf "\t%6s" "max"
    );
    print_char '\n';
  end;

  strides |> IntSet.iter @@ fun stride ->
  let timeslice = if !threads > 0 && timeslice < Float.epsilon then 0.05 (* default OCaml tick thread if we don't set one ourselves *) else timeslice in
  Printf.printf "%8d\t%8.6f\t%8d" !threads timeslice stride;
  let () =
    columns |> IntSet.iter @@ fun n ->
    print_char '\t';
    match Hashtbl.find_opt tbl (stride, n) with
    | Some (result, vmin, vmax) ->
      (* no vmax: otherwise it'll include outliers,
         e.g. where something else runs on the system..
       *)
      Printf.printf "%6.1f\t%6.3f\t%6.3f" result vmin vmax
    | None -> print_string "\t\t"
  in
  print_char '\n'

let nothing _ = Ok ()

let print_bechamel_results (ols, raw_results, results) timeslice =
  let results = analyze' ols raw_results results in
  let open Bechamel_js in
  let name = Printf.sprintf "output%d_%f.json" !threads timeslice in
  Out_channel.with_open_text name @@ fun ch ->
  let res =
    emit ~dst:(Channel ch) nothing ~compare ~x_label:(Measure.label reads)
    ~y_label:(Measure.label instance)
    results
  in
  match res with
  | Ok () -> ()
  | Error (`Msg err) ->
    Format.eprintf "Cannot save results for %s: %s" name err

let raw = ref false

let header = ref []

let print_raw_results ?(print_header=false) raw_results timeslice =
  let label = Measure.label instance in
  let timeslice = if !threads > 0 && timeslice < Float.epsilon then 0.05 (* default OCaml tick thread if we don't set one ourselves *) else timeslice in
  if print_header then begin
    header := List.of_seq (
      raw_results |> Hashtbl.to_seq_keys |> Seq.map @@ fun name ->
      Scanf.sscanf name "strided_read/%d:%d" @@ fun n _stride ->
      n, name) |> List.sort (fun (a, _) (b, _) -> Int.compare a b);
    Printf.printf "TIMESLICE";
    let () = 
      !header |> List.iter @@ fun (n, _) ->
      print_char '\t';
      print_suffix n
    in
    print_char '\n'
  end;
  let rows = (raw_results |> Hashtbl.to_seq_values |> Seq.take 1 |> List.of_seq |> List.hd).Benchmark.lr |> Array.length in
  for i = 0 to rows - 1 do
    Printf.printf "%8.6f" timeslice;
    let () =
     !header |> List.iter @@ fun (_, name) ->
     match Hashtbl.find_opt raw_results name with
     | Some result when i < Array.length result.Benchmark.lr ->
        let m = result.Benchmark.lr.(i) in
        let cycles_per_op = Measurement_raw.get ~label m /. Measurement_raw.get ~label:predictor m in
        Printf.printf "\t%6.3f" cycles_per_op
     | _ -> print_char '\t' (* missing *)
    in
    Printf.printf "\n"
  done

let run_tests ?print_header cfg random timeslice = 
  let f = if random then test_cycle_read else test_strided_read in
  if timeslice > 0. then set_timeslice timeslice;
  let raw_results = tests timeslice f |> Bechamel.Benchmark.all cfg instances in
  if !raw then
    print_raw_results ?print_header raw_results timeslice
  else
    let ols, raw_results, results = raw_results |> analyze in
    print_results ?print_header timeslice results;
    print_bechamel_results (ols, raw_results, results) timeslice

let timeslices =
  let step = 0.5
  and count = 25
  in
  let default_timeslice = 0.05 in
  Seq.ints 1 |> Seq.map (fun i -> default_timeslice *. 2. ** (-. float i *. step))
  |> Seq.take count |> List.of_seq


let () =
  let random = ref false in
  let timeslice = ref 0. in
  let bench_time = ref 0.1 in
  let auto = ref false in
  Arg.parse [
    "--random", Arg.Set random, "Use random rather than sequential reads"
  ; "--threads", Arg.Set_int threads,"Use specified number of threads"
  ; "--timeslice", Arg.Set_float timeslice, "Set thread yield timeslice"
  ; "--benchtime", Arg.Set_float bench_time, "Benchmark minimum time per cell"
  ; "--short", Arg.Set short, "Test just 2 stride sizes"
  ; "--process", Arg.Set process, "Measure latency using process and sockets"
  ; "--auto", Arg.Set auto, "Test multiple timeslices"
  ; "--bootstrap", Arg.Set bootstrap, "Calculate confidence intervals"
  ; "--short-stride-size", Arg.Set_int short_stride_size, "Short stride size (default: 2*linesize)"
  ; "--raw", Arg.Set raw, "Print raw measurements only for boxplot"
  ] ignore "cacheplot [--random]";
  (* 

  TODO: because we defined our own 'loops' below, we don't need a high limit here
  
  (* to allow equal number of memory reads to execute Strided_read.read with stride=N/2,
     and with stride=linesize we need an iteration limit of at least:
     limit = N/linesize/2
     and we might want to repeat this multiple times for accuracy
   *)
  let limit = 100 * (List.hd caches).Cachesize.size / linesize / 2 in
  *)
  let cfg = Bechamel.Benchmark.cfg ~quota:(Time.second !bench_time) ~start:2 ~stabilize:false () in
  if !auto then begin
   short := true;
(*   threads := 0;
   run_tests cfg !random 0.;*)
   threads := 1;
   let () = 
     timeslices |> List.rev
     |> List.iteri @@ fun i timeslice ->
     run_tests ~print_header:(i = 0) cfg !random timeslice;
   in
   run_tests ~print_header:false cfg !random 0.
  end
  else
    run_tests cfg !random !timeslice
  
(* TODO: latency measurements where we make syscalls form one process,
   and wait until the other process replies, that runs the busy thread, and then the rr thread
 *)
