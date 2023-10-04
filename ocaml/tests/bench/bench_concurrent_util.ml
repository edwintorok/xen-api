external parallel_c_work : int -> unit = "caml_bench_concurrent_parallel_c_work"

module type BARRIER = sig
  type t

  val make : int -> t

  val phase1 : t -> unit

  val phase2 : t -> unit

  val wait : t -> unit

  val name: string
end

module BarrierCond = struct
  module Turnstile = struct
    type t =
    { m: Mutex.t
    ; mutable state: bool    
    ; cond: Condition.t
    }

    let create state = {m = Mutex.create(); state; cond = Condition.create ()}

    let wait t =
      Mutex.lock t.m;
      while not t.state do
        Condition.wait t.cond t.m
      done;
      t.state <- false;
      Mutex.unlock t.m
    
    let signal t =
      Mutex.lock t.m;
      assert (not t.state);
      t.state <- true;
      Condition.signal t.cond;
      Mutex.unlock t.m
  end
  
  type t = {
    n: int
  ; count: int Atomic.t
  ; turnstile: Turnstile.t
  ; turnstile2: Turnstile.t
  }
  let name = "barrier(condvars)"

  let make n = {
      n; count = Atomic.make 0; turnstile = Turnstile.create false; turnstile2 = Turnstile.create true
    }

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n);
    if count = t.n then begin
      Turnstile.wait t.turnstile2;
      Turnstile.signal t.turnstile
    end;
    Turnstile.wait t.turnstile;
    Turnstile.signal t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) -1 in
    assert (count >= 0);
    if count = 0 then begin
      Turnstile.wait t.turnstile;
      Turnstile.signal t.turnstile2
    end;
    Turnstile.wait t.turnstile2;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t; phase2 t
  
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

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then
      signal t.turnstile t.n ;
    Semaphore.Counting.acquire t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then
      signal t.turnstile2 t.n ;
    Semaphore.Counting.acquire t.turnstile2

  let wait t = phase1 t ; phase2 t
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

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then begin
      Semaphore.Binary.acquire t.turnstile2;
      Semaphore.Binary.release t.turnstile;
    end;
    Semaphore.Binary.acquire t.turnstile;
    Semaphore.Binary.release t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then begin
      Semaphore.Binary.acquire t.turnstile;
      Semaphore.Binary.release t.turnstile2;
    end;
    Semaphore.Binary.acquire t.turnstile2;
    Semaphore.Binary.release t.turnstile2

  let wait t = phase1 t ; phase2 t
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

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then begin
      Semaphore.Counting.acquire t.turnstile2;
      Semaphore.Counting.release t.turnstile;
    end;
    Semaphore.Counting.acquire t.turnstile;
    Semaphore.Counting.release t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then begin
      Semaphore.Counting.acquire t.turnstile;
      Semaphore.Counting.release t.turnstile2;
    end;
    Semaphore.Counting.acquire t.turnstile2;
    Semaphore.Counting.release t.turnstile2

  let wait t = phase1 t ; phase2 t
end

(* this relies on OCaml per-domain runtime lock allowing only 1 thread at a time to run,
   and therefore we can use Thread.yield to wait for a condition to be reached.
 *)
module BarrierYield = struct
  module Turnstile : sig
    type t
    val create : bool -> t
    val wait : t -> unit
    val signal: t -> unit
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

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n) ;
    if count = t.n then begin
      Turnstile.wait t.turnstile2;
      Turnstile.signal t.turnstile;
    end;
    Turnstile.wait t.turnstile;
    Turnstile.signal t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0) ;
    if count = 0 then begin
      Turnstile.wait t.turnstile;
      Turnstile.signal t.turnstile2;
    end;
    Turnstile.wait t.turnstile2;
    Turnstile.signal t.turnstile2

  let wait t = phase1 t ; phase2 t
end
