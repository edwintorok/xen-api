open Bechamel

let run_parallel_c_work () = Bench_concurrent_util.parallel_c_work 10

module TestBarrier (B : Bench_concurrent_util.BARRIER) = struct
  let make_barrier_threads n =
    let barrier = B.make (n + 1) in
    let stop = Atomic.make false in
    ( barrier
    , stop
    , ref false
    , Array.init n
      @@ Thread.create
      @@ fun i ->
      let local_stop = ref false in
      while not !local_stop do
        B.phase1 barrier i ;
        if Atomic.get stop then
          local_stop := true ;
        B.phase2 barrier i
      done
      (* Format.eprintf "[%d]: exit\n" (Thread.id (Thread.self ())); flush stderr*)
    )

  let free_barrier_threads (barrier, stop, freed, threads) =
    assert (not !freed) ;
    freed := true ;
    Atomic.set stop true ;
    B.phase1 barrier (Array.length threads) ;
    B.phase2 barrier (Array.length threads) ;
    Array.iter
      (fun t ->
        (*Format.eprintf "[0]: join %d\n" (Thread.id t);
          flush stderr;*)
        Thread.join t
        (*Format.eprintf "[0]: joined %d\n" (Thread.id t);
          flush stderr;*)
      )
      threads

  let test =
    Test.make_indexed_with_resource ~args:[1; 4; 8; 16] ~name:B.name
      Test.multiple ~allocate:make_barrier_threads ~free:free_barrier_threads
      (fun _ -> Staged.stage @@ fun (barrier, _, _, _) -> B.wait barrier
    )
end

let event_pingpong_allocate () =
  let e1 = Event.new_channel () and e2 = Event.new_channel () in
  let t =
    ()
    |> Thread.create @@ fun () ->
       while Event.(sync @@ receive e1) do
         Event.(sync @@ send e2 ())
       done
  in

  (e1, e2, t)

let event_pingpong_free (e1, _, t) =
  Event.(sync (send e1 false)) ;
  Thread.join t

let event_pingpong_run (e1, e2, _) =
  Event.(sync @@ send e1 true) ;
  Event.(sync @@ receive e2)

let condvar_pingpong_allocate () =
  let m = Mutex.create () in
  let cond1 = Condition.create () and cond2 = Condition.create () in
  let go = ref false and stop = ref false in
  let t =
    ()
    |> Thread.create @@ fun () ->
       Mutex.lock m ;
       while not !stop do
         while not !go do
           Condition.wait cond1 m
         done ;
         Mutex.unlock m ;

         (* would run actual benchmark code here, mutex has to be released! *)
         Mutex.lock m ;
         go := false ;
         Condition.signal cond2
       done ;
       Mutex.unlock m
  in
  (m, cond1, cond2, stop, go, t)

let condvar_pingpong_free (m, cond1, _, stop, go, t) =
  Mutex.lock m ;
  stop := true ;
  go := true ;
  Condition.signal cond1 ;
  Mutex.unlock m ;
  Thread.join t

let condvar_pingpong_run (m, cond1, cond2, _, go, _) =
  Mutex.lock m ;
  go := true ;
  Condition.signal cond1 ;
  while !go do
    Condition.wait cond2 m
  done ;
  Mutex.unlock m

let make_barriercond_threads n =
  Array.init n @@ fun _ -> condvar_pingpong_allocate ()

let free_barriercond_threads a = Array.iter condvar_pingpong_free a

let run_barriercond_threads a =
  (* wake all *)
  let () =
    a
    |> Array.iter @@ fun (m, cond1, _cond2, _, go, _) ->
       (* this allows some threads to start sooner than others, which is fine,
           the goal here is to avoid thread creation overhead and run benchmarks in already running threads instead
       *)
       Mutex.lock m ;
       go := true ;
       Mutex.unlock m ;
       Condition.signal cond1
  in
  (* wait for all to finish *)
  let () =
    a
    |> Array.iter @@ fun (m, _cond1, cond2, _, go, _) ->
       (* this allows some threads to start sooner than others, which is fine,
           the goal here is to avoid thread creation overhead and run benchmarks in already running threads instead
       *)
       Mutex.lock m ;
       while !go do
         Condition.wait cond2 m
       done ;
       Mutex.unlock m
  in
  ()

let benchmarks =
  Test.make_grouped ~name:"Concurrent"
    [
      Test.make ~name:"overhead" (Staged.stage ignore)
    ; Test.make ~name:"parallel_c_work(10ms)" (Staged.stage run_parallel_c_work)
    ; Bench_concurrent_util.test_concurrently ~name:"parallel_c_work"
        ~allocate:ignore ~free:ignore (fun _ -> Staged.stage run_parallel_c_work
      )
      (* ; (let module T = TestBarrier (Bench_concurrent_util.BarrierPreloaded) in
           T.test
           )
         ; (let module T = TestBarrier (Bench_concurrent_util.BarrierCounting) in
           T.test
           )
         ; (let module T = TestBarrier (Bench_concurrent_util.BarrierBinary) in
           T.test
           )
         ; (let module T = TestBarrier (Bench_concurrent_util.BarrierCond) in
           T.test
           )
         (*; (let module T = TestBarrier (Bench_concurrent_util.BarrierYield) in
           T.test
           )*)
         ; (let module T = TestBarrier (Bench_concurrent_util.BarrierBinaryArray) in
           T.test
           )
         ; Test.make_indexed ~args:[1; 4; 8; 16] ~name:"Thread create/join" (fun n ->
               Staged.stage @@ fun () ->
               let threads = Array.init n @@ Thread.create ignore in
               Array.iter Thread.join threads
           )
         ; Test.make_with_resource ~name:"Event ping/pong"
             ~allocate:event_pingpong_allocate ~free:event_pingpong_free
             Test.multiple
             (Staged.stage event_pingpong_run)
         ; Test.make_with_resource ~name:"Condvar pingpong"
             ~allocate:condvar_pingpong_allocate ~free:condvar_pingpong_free
             Test.multiple
             (Staged.stage condvar_pingpong_run)
         ; Test.make_indexed_with_resource ~args:[1; 4; 8; 16]
             ~name:"barrier (condvar array)" Test.multiple
             ~allocate:make_barriercond_threads ~free:free_barriercond_threads
             (fun _ -> Staged.stage run_barriercond_threads
           )*)
    ]

let () = Bechamel_simple_cli.cli benchmarks
