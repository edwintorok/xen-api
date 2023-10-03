open Bechamel

let run_parallel_c_work () = Bench_concurrent_util.parallel_c_work 10

let make_barrier_threads n =
  let barrier = Bench_concurrent_util.Barrier.make (n+1) in
  let stop = Atomic.make false in
  barrier, stop, Array.init n @@ Thread.create @@ fun _ ->
  let local_stop = ref false in
  while not !local_stop do
    Bench_concurrent_util.Barrier.phase1 barrier;
    Bench_concurrent_util.Barrier.phase2 barrier;
    if Atomic.get stop then
      local_stop := true;
  done

let free_barrier_threads (barrier, stop, threads) =
  Bench_concurrent_util.Barrier.phase1 barrier;
  Atomic.set stop true;
  Bench_concurrent_util.Barrier.phase2 barrier;
  Array.iter Thread.join threads

let benchmarks =
  Test.make_grouped ~name:"Concurrent"
    [
    (*  Test.make ~name:"overhead" (Staged.stage ignore)
    ; Test.make ~name:"parallel_c_work(10ms)" (Staged.stage run_parallel_c_work)*)
     Test.make_indexed_with_resource ~args:[1;4;]
        ~name:"barrier (semaphores)"
        Test.uniq
        ~allocate:make_barrier_threads
        ~free:free_barrier_threads
        (fun _ -> Staged.stage @@ fun (barrier,_, _) ->
    (*      Bench_concurrent_util.Barrier.wait barrier*)()
        )
  
    ]

let () = Bechamel_simple_cli.cli benchmarks
