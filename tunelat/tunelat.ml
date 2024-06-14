open Simple_workloads

(*
  L2 sequential cache miss penalty = n=(2*L2) read time / 2 - L2 read time
  L2 sequential cached read

  timeslice: 1st one will miss, subsequent ones not

  L2_full_cached_time + L2_full_miss_penalty + L2_full_cached_time + .. = timeslice

  perf loss = L2_full_miss_penalty / timeslice <= 2.5% ->

  this is for Xen...
  L2_full_miss_penalty / 2.5% <= timeslice --> 5.4ms?
  (will need also CAT for LLC?)

 
*)


let stop = Atomic.make false
let worker () =
  while not (Atomic.get stop) do
    Operations.Yield.perform ()
  done

let max_overhead_percentage = 0.5

let measure_yield_overhead () =
  let t = Thread.create worker () in
  let yield_lo, yield_overhead, yield_hi = Simple_measure.measure Operations.Yield.perform Operations.Yield.count in
  Printf.printf "Yield overhead: %.7fs,%.7fs,%.7fs\n" yield_lo yield_overhead yield_hi;
  Atomic.set stop true;
  Thread.join t;
  yield_overhead

let measure_caches () =
  let caches = Cachesize.caches () in
  let sizes = List.map (fun c -> c.Cachesize.size) caches in
  let sizes = List.hd sizes * 2 :: sizes in
  let linesize = List.fold_left Int.max 0 (List.map (fun c -> c.Cachesize.linesize) caches) in

  let stride_size_bytes = linesize in
  let timings =
    sizes |> List.rev_map @@ fun size ->
    let data = Operations.CycleRead.allocate ~stride_size_bytes size in
    Gc.full_major ();
    Simple_measure.measure_min ~n:10 Operations.CycleRead.read data /. (float size /. float stride_size_bytes)
  in
  let overheads =
    (* not quite right, compare just to prev? *)
(*    let op_overhead = List.hd timings in
    List.map (fun t -> t -. op_overhead) (List.tl timings)  *)
    let next = List.tl timings |> List.to_seq in
    let prev = List.to_seq timings in
    Seq.map2 (-.) next prev |> List.of_seq
    
  in
  let overheads_time =
    Seq.map2 (fun n t -> float (n / stride_size_bytes) *. t) (List.rev sizes |> List.to_seq) (List.to_seq overheads)
    |> List.of_seq
  in
  List.iteri (fun i t -> Printf.printf "%d: %.9f/op\n" (i+1) t) timings;
  List.iteri (fun i t -> Printf.printf "L%d cache miss: %.9f/op\n" (i+1) t) overheads;
  List.iteri (fun i t -> Printf.printf "L%d cache miss: %.9fs\n" (i+1) t) overheads_time

let () =
  let yield_overhead = measure_yield_overhead () in
  measure_caches ();

  let timeslice_min = yield_overhead /. (max_overhead_percentage /. 100.) in
  Printf.printf "timeslice: %.6fs\n" timeslice_min

