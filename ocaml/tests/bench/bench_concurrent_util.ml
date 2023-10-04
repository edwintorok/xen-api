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
  type t = {
    n: int
  ; mutable count: int
  ; m: Mutex.t
  ; mutable go: bool
  ; turnstile: Condition.t
  ; mutable go2: bool
  ; turnstile2: Condition.t
  }
  let name = "barrier(condvars)"

  let make n = {
      n; count = 0; m = Mutex.create (); turnstile = Condition.create (); go = false; go2 = false; turnstile2 = Condition.create ()
    }

  let phase1 t =
    Mutex.lock t.m;
      t.count <- t.count + 1;
      assert (t.count <= t.n);
      if t.count = t.n then begin
        t.go <- true;
        t.go2 <- false
      end
      else while not t.go do
          Condition.wait t.turnstile t.m
    done;
    Mutex.unlock t.m;
    (* wake up anyone else who is waiting *)
    Condition.broadcast t.turnstile

  let phase2 t =
    Mutex.lock t.m;
      t.count <- t.count - 1;
      assert (t.count >= 0);
      if t.count = 0 then begin
        t.go2 <- true;
        t.go <- false
      end
      else while not t.go2 do
          Condition.wait t.turnstile2 t.m
    done;
    Mutex.unlock t.m;
    (* wake up anyone else who is waiting *)
    Condition.broadcast t.turnstile2

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
