(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module type SIZE = sig
  val n : unit -> int
end

module Make (Size : SIZE) = struct
  module Semaphore = Xapi_stdext_threads.Semaphore

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let semaphore = ref None

  let m = Mutex.create ()

  let get_semaphore () =
    with_lock m @@ fun () ->
    match !semaphore with
    | None ->
        let result = Semaphore.create (Size.n ()) in
        semaphore := Some result ;
        result
    | Some s ->
        s

  let execute f = Semaphore.execute (get_semaphore ()) f
end

module Batching = struct
  type t = {delay_before: float; delay_between: float}

  type arg = float

  let make ~delay_before ~delay_between = {delay_before; delay_between}

  (** [perform_delay delay] calls {!val:Thread.delay} when [delay] is non-zero.

    To avoid issues with floating-point rounding, we consider everything smaller than {!val:Float.epsilon} equivalent to 0.
    Thread.delay 0 provides no fairness guarantees, the current thread may actually be the one that gets the global lock again.
    Instead {!val:Thread.yield} could be used, which does provide fairness guarantees, but it may also introduce large latencies
    when there are lots of threads waiting for the OCaml runtime lock. Only invoke this once, in the [delay_before] section.
   *)
  let perform_delay ~yield delay =
    if delay > Float.epsilon then
      Thread.delay delay
    else if yield then
      (* this is a low-priority thread, if there are any other threads waiting, then run them now.
         If there are no threads waiting then this a noop.
         Requires OCaml >= 4.09 (older versions had fairness issues in Thread.yield)
      *)
      Thread.yield ()

  let with_recursive config f () =
    let rec self arg =
      let arg = Float.min config.delay_between (arg *. 2.) in
      perform_delay ~yield:false arg ;
      (f [@tailcall]) self arg
    in
    let self0 arg = (f [@tailcall]) self arg in
    perform_delay ~yield:true config.delay_before ;
    f self0 (config.delay_between /. 16.)
end

module Rusage = struct
  (** We have individual Atomic fields,
      instead of a tuple/record inside a single Atomic to avoid allocating while updating the values

      Atomic is used to future-proof for OCaml 5 (on OCaml 4 plain 'ref' would suffice).
   *)

  type t = {
      name: string  (** name of what we are measuring *)
    ; updating: int Atomic.t  (** mark the record as being updated *)
    ; usage_ns: int Atomic.t  (** CPU usage in nanoseconds *)
    ; mtime_ns: int Atomic.t  (** elapsed time in nanoseconds, including idle *)
    ; count: int Atomic.t  (** number of calls *)
  }

  (** update measurements lock-free and without allocating,
      to keep overheads minimal in the code that we measure
   *)
  let atomic_update t ~delta_usage_ns ~delta_mtime_ns =
    (* must be first *)
    Atomic.incr t.updating ;

    let (_ : int) = Atomic.fetch_and_add t.usage_ns delta_usage_ns
    and (_ : int) = Atomic.fetch_and_add t.mtime_ns delta_mtime_ns in
    Atomic.incr t.count ;

    (* must be last *)
    Atomic.decr t.updating

  (** read a consistent sample of the measurements,
      this is not performance critical code, and allocates the result
   *)
  let rec atomic_sample t =
    let updating = Atomic.get (Sys.opaque_identity t.updating) in
    let count = Atomic.get (Sys.opaque_identity t.count) in
    let usage_ns = Atomic.get t.usage_ns
    and mtime_ns = Atomic.get t.mtime_ns
    and count' = Atomic.get (Sys.opaque_identity t.count) in
    let updating' = Atomic.get (Sys.opaque_identity t.updating) in
    if count <> count' || updating <> 0 || updating' <> 0 then (
      (* race condition (can only happen on OCaml 5):
           the generation counter has changed while we were reading the values,
           or other thread(s) have started updating the values, even if we haven't
           observed a change in the generation counter yet. *)
      Thread.yield () ;
      (* Domain.cpu_relax on OCaml 5 *)
      atomic_sample t
    ) else
      (usage_ns, mtime_ns, count)

  (** initialie a measurement record *)
  let make name =
    {
      name
    ; updating= Atomic.make 0
    ; usage_ns= Atomic.make 0
    ; mtime_ns= Atomic.make 0
    ; count= Atomic.make 0
    }

  (* get CPU resource usage without allocating,
     at least on native builds

     the conversion to int on 64-bit is safe here,
     it won't overflow for >100 years
  *)
  let get_rusage_thread_ns () =
    Rusage_thread.getrusage_thread_ns () |> Int64.to_int

  (* we use Monotonic_clock instead of Mtime_clock,
     due to the unboxed int64 result.
  *)
  let get_mtime_ns () = Monotonic_clock.now () |> Int64.to_int

  let measure_complete t u0 m0 =
    let u1 = get_rusage_thread_ns () and m1 = get_mtime_ns () in
    let delta_usage_ns = u1 - u0 and delta_mtime_ns = m1 - m0 in
    atomic_update t ~delta_usage_ns ~delta_mtime_ns

  (** measure resource usage of [f ()] with very low overhead *)
  let measure_rusage t f arg =
    let u0 = get_rusage_thread_ns () and m0 = get_mtime_ns () in
    (* Fun.protect would allocate, implement directly instead *)
    match f arg with
    | r ->
        measure_complete t u0 m0 ; r
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        measure_complete t u0 m0 ;
        Printexc.raise_with_backtrace e bt

  let float_of_ns ns = float ns *. 1e-9

  (** read measurements *)
  let sample t =
    let usage_ns, mtime_ns, count = atomic_sample t in
    (float_of_ns usage_ns, float_of_ns mtime_ns, count)
end
