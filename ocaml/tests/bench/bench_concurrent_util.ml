external parallel_c_work : int -> unit = "caml_bench_concurrent_parallel_c_work"

(* See "The Little Book of Semaphores" 3.7 Reusable barrier, 3.7.7 Barrier objects.
   Instead of the mutex+count we use an atomic though
 *)
module Barrier = struct
  type t =
  { n: int
  ; count: int Atomic.t
  ; turnstile: Semaphore.Counting.t
  ; turnstile2: Semaphore.Counting.t
  }

  let make n =
    { n; count = Atomic.make 0; turnstile = Semaphore.Counting.make 0; turnstile2 = Semaphore.Counting.make 0}

  let signal semaphore n =
    for _ = 1 to n do
      Semaphore.Counting.release semaphore
    done

  let phase1 t =
    let count = 1 + Atomic.fetch_and_add t.count 1 in
    assert (count <= t.n);
    if count = t.n then
      signal t.turnstile t.n;
    Semaphore.Counting.acquire t.turnstile

  let phase2 t =
    let count = Atomic.fetch_and_add t.count (-1) - 1 in
    assert (count >= 0);
    if count = 0 then
      signal t.turnstile2 t.n;
    Semaphore.Counting.acquire t.turnstile2

  let wait t = phase1 t; phase2 t
end
