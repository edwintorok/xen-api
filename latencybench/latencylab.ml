(* TODO: use multicore-bench when ready *)

(* based on bechamel example code *)
open Bechamel
open Toolkit

(* fixme: this is a hack, doesn't work)
let measurables = Measures.Measurable.mone ::  Measures.Pmu.measurables
let instances = List.map Measures.Measurable.instance measurables
*)

let instances = [ Bechamel_perf.Instance.cache_misses; Bechamel_perf.Instance.cache_references; Instance.monotonic_clock]

let benchmark tests =
  let cfg = Benchmark.cfg ~stabilize:false () in
  Benchmark.all cfg instances tests

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:[|Measure.run|]
  in
  (*let results =    
    List.map (fun measurable ->
      raw_results
      |> Hashtbl.to_seq
      |> Seq.map (fun (k, v) ->
        k, Measures.Measurable.ols measurable ~bootstrap:0 v
      ) |> Hashtbl.of_seq
    ) measurables
  in*)
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  (Analyze.merge ols instances results, raw_results)

let () =
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let cli tests =
  Format.printf "@,Running benchmarks@." ;
  let results, _ = tests |> benchmark |> analyze in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image

(* TODO: move *)
let worker (wait, loop) =
  Processor.Affinity.set_cpus [Processor.Topology.t |> List.tl |> List.hd];
  Mutex.lock wait;
  Mutex.unlock wait;
  loop ()
 

(* TODO: better base this on cachemeasure.ml *)

let () =
  cli (
    Test.make_indexed_with_resource
      ~name:"timeslice"
      ~args:[75; 1000; 2000; 4000; 8000; 16000; 32000]
      ~allocate:(fun timeslice_us ->
        let timeslice = float timeslice_us *. 1e-6 in
        Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle (fun _ -> Thread.yield ()));
        let (_ : Unix.interval_timer_status) =
          Unix.setitimer Unix.ITIMER_VIRTUAL Unix.{it_interval = timeslice; it_value = timeslice}
        in
        let loop, shutdown = Workloads.Cpu.cache_misses2 () in
        let init, work = Workloads.Cpu.cache_misses2' () in
        let wait = Mutex.create () in
        Mutex.lock wait;
        wait, ref true, init (), work, shutdown, Array.init 8 @@ fun _ -> Thread.create worker (wait, loop)
      )
      ~free:(fun (_, _, _,_, shutdown, threads) -> shutdown (); Array.iter Thread.join threads)
      Test.uniq
      @@ fun timeslice_us ->
        Staged.stage (fun (wait, locked, a, work, _, threads) ->
          Processor.Affinity.set_cpus [Processor.Topology.t |> List.tl |> List.hd];
          if !locked then begin
            Mutex.unlock wait;
            locked := false
          end;
          work a;
        )
  )
    
