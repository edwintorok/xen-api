open Bechamel

module type BARRIER = sig
  type t

  val make : int -> t

  val phase1 : t -> int -> unit

  val phase2 : t -> int -> unit

  val wait : t -> unit

  val name : string
end

module BarrierCond = struct
  module Turnstile = struct
    type t = {m: Mutex.t; mutable state: bool; cond: Condition.t}

    let create state = {m= Mutex.create (); state; cond= Condition.create ()}

    let wait t =
      Mutex.lock t.m ;
      while not t.state do
        Condition.wait t.cond t.m
      done ;
      t.state <- false ;
      Mutex.unlock t.m

    let signal t =
      Mutex.lock t.m ;
      assert (not t.state) ;
      t.state <- true ;
      Condition.signal t.cond ;
      Mutex.unlock t.m
  end

  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Turnstile.t
    ; turnstile2: Turnstile.t
  }

  let name = "barrier(condvars)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Turnstile.create false
    ; turnstile2= Turnstile.create true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Turnstile.wait t.turnstile2 ;
      Turnstile.signal t.turnstile
    ) ;
    Turnstile.wait t.turnstile ;
    Turnstile.signal t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Turnstile.wait t.turnstile ;
      Turnstile.signal t.turnstile2
    ) ;
    Turnstile.wait t.turnstile2 ;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

(* See "The Little Book of Semaphores" 3.7 Reusable barrier, 3.7.7 Barrier objects.
   Instead of the mutex+count we use an atomic though
*)
module BarrierPreloaded = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Counting.t
    ; turnstile2: Semaphore.Counting.t
  }

  let name = "barrier(semaphores,preloaded)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Counting.make 0
    ; turnstile2= Semaphore.Counting.make 0
    }

  let signal semaphore n =
    for _ = 1 to n do
      Semaphore.Counting.release semaphore
    done

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then
      signal t.turnstile t.n ;
    Semaphore.Counting.acquire t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then
      signal t.turnstile2 t.n ;
    Semaphore.Counting.acquire t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierBinary = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Binary.t
    ; turnstile2: Semaphore.Binary.t
  }

  let name = "barrier(semaphores,binary)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Binary.make false
    ; turnstile2= Semaphore.Binary.make true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Semaphore.Binary.acquire t.turnstile2 ;
      Semaphore.Binary.release t.turnstile
    ) ;
    Semaphore.Binary.acquire t.turnstile ;
    Semaphore.Binary.release t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Semaphore.Binary.acquire t.turnstile ;
      Semaphore.Binary.release t.turnstile2
    ) ;
    Semaphore.Binary.acquire t.turnstile2 ;
    Semaphore.Binary.release t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierCounting = struct
  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Semaphore.Counting.t
    ; turnstile2: Semaphore.Counting.t
  }

  let name = "barrier(semaphores,counting)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Semaphore.Counting.make 0
    ; turnstile2= Semaphore.Counting.make 1
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Semaphore.Counting.acquire t.turnstile2 ;
      Semaphore.Counting.release t.turnstile
    ) ;
    Semaphore.Counting.acquire t.turnstile ;
    Semaphore.Counting.release t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Semaphore.Counting.acquire t.turnstile ;
      Semaphore.Counting.release t.turnstile2
    ) ;
    Semaphore.Counting.acquire t.turnstile2 ;
    Semaphore.Counting.release t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

(* this relies on OCaml per-domain runtime lock allowing only 1 thread at a time to run,
    and therefore we can use Thread.yield to wait for a condition to be reached.
   This is very inefficient, just for comparison: we have no control over which thread wakes:
   it could be one that will immediately yield again
*)
module BarrierYield = struct
  module Turnstile : sig
    type t

    val create : bool -> t

    val wait : t -> unit

    val signal : t -> unit
  end = struct
    type t = bool Atomic.t

    let create = Atomic.make

    let wait t =
      while not (Atomic.compare_and_set t true false) do
        Thread.yield ()
      done

    let signal t =
      let ok = Atomic.compare_and_set t false true in
      assert ok
  end

  type t = {
      n: int
    ; count: int Atomic.t
    ; turnstile: Turnstile.t
    ; turnstile2: Turnstile.t
  }

  let name = "barrier(atomic,yield)"

  let make n =
    {
      n
    ; count= Atomic.make 0
    ; turnstile= Turnstile.create false
    ; turnstile2= Turnstile.create true
    }

  let phase1 t _ =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then (
      Turnstile.wait t.turnstile2 ;
      Turnstile.signal t.turnstile
    ) ;
    Turnstile.wait t.turnstile ;
    Turnstile.signal t.turnstile

  let phase2 t _ =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then (
      Turnstile.wait t.turnstile ;
      Turnstile.signal t.turnstile2
    ) ;
    Turnstile.wait t.turnstile2 ;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t () ; phase2 t ()
end

module BarrierBinaryArray = struct
  module Turnstile = struct
    type t = {start: Semaphore.Binary.t; stop: Semaphore.Binary.t}

    let create _ =
      {start= Semaphore.Binary.make false; stop= Semaphore.Binary.make false}

    let wait_start t = Semaphore.Binary.acquire t.start

    let wake_start t = Semaphore.Binary.release t.start

    let wait_stop t = Semaphore.Binary.acquire t.stop

    let wake_stop t = Semaphore.Binary.release t.stop
  end

  type t = Turnstile.t Array.t

  let name = "barrier(semaphores,binary,array)"

  let make n = Array.init (n - 1) Turnstile.create

  let m = Mutex.create ()
  (*
  let k s =
    let id =  Thread.id (Thread.self ()) in
    Mutex.lock m;
    Format.eprintf "[%d]: %s\n" id s;
    flush stderr;
    Mutex.unlock m
    *)

  let phase1 t i =
    if i < Array.length t then
      (*
      Format.ksprintf k "%d/%d: wait_start@." i (Array.length t);*)
      Turnstile.wait_start t.(i)
    else
      (*
      Format.ksprintf k "%d: start@." i;*)

      (*
      Format.ksprintf k "n: wake_start@.";*)
      Array.iter Turnstile.wake_start t
  (*
      Format.ksprintf k "n: woke_start@.";*)

  let phase2 t i =
    if i < Array.length t then (*
      Format.ksprintf k "%d: wake_stop@." i;*)
      Turnstile.wake_stop t.(i)
    else
      (*
      Format.ksprintf k "%d: woke_stop@." i*)

      (*
      Format.ksprintf k "n: wait_stop@.";*)
      Array.iter Turnstile.wait_stop t (*
      Format.ksprintf k "n: stop@."*)

  let wait t =
    phase1 t (Array.length t) ;
    phase2 t (Array.length t)
end

let run_parallel_c_work () = Bench_concurrent_util.parallel_c_work 10

module TestBarrier (B : BARRIER) = struct
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
      Bench_concurrent_util.test_concurrently ~name:"parallel_c_work"
        ~allocate:ignore ~free:ignore
        (Staged.stage run_parallel_c_work)
    ; Bench_concurrent_util.test_concurrently ~name:"pam"
        ~allocate:Pam.authenticate_start ~free:Pam.authenticate_stop
        ( Staged.stage @@ fun pam ->
          Pam.authorize pam "pamtest-edvint" "pamtest-edvint"
        )
    ; Test.make ~name:"overhead" (Staged.stage ignore)
    ; Test.make ~name:"parallel_c_work(10ms)" (Staged.stage run_parallel_c_work)
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
