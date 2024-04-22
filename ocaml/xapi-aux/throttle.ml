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

  let name t = t.name

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

module Controller = struct
  type stats = {
      avg_cpu_used_seconds: float
    ; cpu_used_percentage: float
    ; elapsed_seconds: float
  }

  type t = {
      max_cpu_usage: float
    ; busy: float
    ; elapsed: float
    ; delay: float
    ; min_delay: float
  }

  let update_average average next =
    (* exponentially weighted moving average to smooth measurementss,
        we could eventually use something more sophisticated like a Kalman filter here.

       We don't want to use a cumulative average, because temporary spikes in usage shouldn't forever slow down Event.from
    *)
    (average *. 0.5) +. (next *. 0.5)

  let make ~max_cpu_usage ~delay_before ~delay_between =
    {
      max_cpu_usage
    ; busy= 0.
    ; elapsed= 0.
    ; delay= 0.
    ; min_delay= delay_before +. delay_between
    }

  (*
    Worst case with calling this API in a tight loop, from a single caller:
      cpu_usage = avg_cpu_used_seconds / elapsed_seconds
    Actual (with delays between API calls themselves, or potentially concurrent calls):
      cpu_used_percentage


    Assuming 2 concurrent calls:
      call1: |---(delay + delay before)---|-(wait for DB event)-|-(CPU)-|-(delay between)-|-(CPU)-|
      call2: |---(delay + delay before)---|-(wait for DB event)-|.......|.-(CPU)-|-(delay between)-|-(CPU)-|

      elapsed(t) = cpu_used(t) + idle(t) + delay(t)
      elapsed(t-1) = cpu_used(t-1) + idle(t-1) + delay(t-1)

      delay(t) = delay(t-1) + cpu_used(t) / desired_usage - elapsed(t)      

      
   *)

  let update t stats =
    let t =
      {
        t with
        busy= update_average t.busy stats.avg_cpu_used_seconds
      ; elapsed= update_average t.elapsed stats.elapsed_seconds
      }
    in
    {t with delay= t.delay +. (t.busy /. t.max_cpu_usage) -. t.elapsed}

  let get_delay t = Float.max t.delay 0.
  (* (t.average /. t.max_cpu_usage) -. t.average -. t.min_delay |> Float.max 0. *)
end

module Limit = struct
  type sample = {
      since: Mtime_clock.counter
    ; sample: float * float * int
    ; stats: Controller.stats
    ; controller: Controller.t
    ; delay: float
  }

  type t = {measure: Rusage.t; last: sample Atomic.t}

  let all = ref []

  let register t = all := (Rusage.name t.measure, t) :: !all

  let make measure controller =
    let stats =
      Controller.
        {avg_cpu_used_seconds= 0.; cpu_used_percentage= 0.; elapsed_seconds= 0.}
    in
    let last =
      {
        since= Mtime_clock.counter ()
      ; sample= (0., 0., 0)
      ; stats
      ; controller
      ; delay= 0.
      }
    in
    let t = {measure; last= Atomic.make last} in
    register t ; t

  let create ?(max_cpu_usage = 0.1) ~delay_before ~delay_between name =
    let controller =
      Controller.make ~max_cpu_usage ~delay_before ~delay_between
    in
    let usage = Rusage.make name in
    make usage controller

  let update t =
    let last = Atomic.get t.last in
    let span = Mtime_clock.count last.since in
    let sample_interval = Mtime.Span.to_float_ns span *. 1e-9 in
    let ((rusage1, mtime1, calls1) as sample) = Rusage.sample t.measure
    and rusage0, mtime0, calls0 = last.sample in
    let cpu_used_seconds = rusage1 -. rusage0 in
    let delta_calls = calls1 - calls0 in
    let delta_mtime = mtime1 -. mtime0 in
    let controller, stats, delay =
      if delta_calls > 0 then
        let stats =
          Controller.
            {
              avg_cpu_used_seconds= cpu_used_seconds /. (delta_calls |> float)
            ; cpu_used_percentage= cpu_used_seconds /. sample_interval
            ; elapsed_seconds= delta_mtime /. (delta_calls |> float)
            }
        in
        let controller = Controller.update last.controller stats in
        let delay = Controller.get_delay controller in
        (controller, stats, delay)
      else
        (last.controller, last.stats, last.delay)
    in
    let since = Mtime_clock.counter () in
    let (_ : bool) =
      Atomic.compare_and_set t.last last
        {sample; controller; since; stats; delay}
    in
    (* If we raced with another thread (can only happen on OCaml 5), then do nothing and keep their update.
       If this function is only called from the xapi periodic scheduler then there is no race.
    *)
    ()

  let stats t =
    let last = Atomic.get t.last in
    (last.stats, last.delay)

  let all_stats () = !all |> List.map (fun (k, v) -> (k, stats v))

  let with_limit t f arg =
    let last = Atomic.get t.last in
    if last.delay > Float.epsilon then
      Rusage.measure_rusage t.measure Thread.delay last.delay ;
    Rusage.measure_rusage t.measure f arg
end
