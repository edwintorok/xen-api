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
  let perform_delay ~yield compute =
    let delay = compute () in
    if delay > Float.epsilon then
      Thread.delay delay
    else if yield then
      (* this is a low-priority thread, if there are any other threads waiting, then run them now.
         If there are no threads waiting then this a noop.
         Requires OCaml >= 4.09 (older versions had fairness issues in Thread.yield)
      *)
      Thread.yield ()

  let with_recursive ~delay_before ~delay_after f arg =
    let rec self arg =
      perform_delay ~yield:false delay_after ;
      (f [@tailcall]) self arg
    in
    perform_delay ~yield:true delay_before ;
    f self arg
end

module Rusage = struct
  type t = {
      name: string  (** name of what we are measuring *)
    ; ns: int Atomic.t  (** CPU usage in nanoseconds *)
    ; count: int Atomic.t  (** number of calls *)
  }

  let make name = {name; ns= Atomic.make 0; count= Atomic.make 0}

  let name t = t.name

  let measure_rusage t f =
    let u0 = Rusage_thread.getrusage_thread () in
    let finally () =
      let delta = Rusage_thread.getrusage_thread () -. u0 in
      (* Convert to microseconds, so we can use [fetch_and_add].
         On 64-bit systems this won't overflow for >100 years.
      *)
      let delta_ns = Float.ceil (delta *. 1e+9) |> Float.to_int in
      (* there are multiple threads, so we can't use Atomic.set to update the global measurement,
         we have to update it with the difference *)
      let (_ : int) = Atomic.fetch_and_add t.ns delta_ns in
      Atomic.incr t.count
    in
    Fun.protect ~finally f

  (* there is a race condition here, count may be off by one, but this is only an approximation anyway,
     avoiding locks is probably better than avoiding the tiny race condition window
  *)
  let sample t = (float (Atomic.get t.ns) *. 1e-9, Atomic.get t.count)
end

module Controller = struct
  type stats = {avg_cpu_used_seconds: float; cpu_used_percentage: float}

  type t = {max_cpu_percentage: float; average: float; min_delay: float}

  let update_average t next =
    (* exponentially weighted moving average,
        we could eventually use something more sophisticated like a Kalman filter here.

       We use a moving average because Event.from has a timeout of 30s, so
       sampling at 5s intervals may sometimes show 0% cpu usage, but we need to look at longer term trends.

       We don't want to use a cumulative average, because temporary spikes in usage shouldn't forever slow down Event.from

       The CPU usage over 6 intervals needs to be 1/6 of the initial one, this is approximately true with the formula below
       (but delayed by one measurement interval)
    *)
    (t.average *. 0.21121) +. (next *. 0.78879)

  let make ~max_cpu_percentage ~delay_before ~delay_after =
    {max_cpu_percentage; average= 0.; min_delay= delay_before +. delay_after}

  let update t stats =
    {t with average= update_average t stats.avg_cpu_used_seconds}

  let get_delay t =
    (t.average /. t.max_cpu_percentage) -. t.average -. t.min_delay
    |> Float.max 0.
end

module Limit = struct
  type sample = {
      since: Mtime_clock.counter
    ; sample: float * int
    ; stats: Controller.stats
    ; controller: Controller.t
    ; delay: float
  }

  type t = {measure: Rusage.t; last: sample Atomic.t}

  let all = ref []

  let register t = all := (Rusage.name t.measure, t) :: !all

  let make measure controller =
    let stats =
      Controller.{avg_cpu_used_seconds= 0.; cpu_used_percentage= 0.}
    in
    let last =
      {
        since= Mtime_clock.counter ()
      ; sample= (0., 0)
      ; stats
      ; controller
      ; delay= 0.
      }
    in
    let t = {measure; last= Atomic.make last} in
    register t ; t

  let create ?(max_cpu_percentage=0.1) ~delay_before ~delay_after name =
    let controller =  Controller.make ~max_cpu_percentage ~delay_before ~delay_after in
    let usage = Rusage.make name in
    make usage controller

  let update_interval = Mtime.Span.(5 * s)

  let update t span last =
    let sample_interval = Mtime.Span.to_float_ns span *. 1e-9 in
    let ((rusage1, calls1) as sample) = Rusage.sample t.measure
    and rusage0, calls0 = last.sample in
    let cpu_used_seconds = rusage1 -. rusage0 in
    let stats =
      Controller.
        {
          avg_cpu_used_seconds= cpu_used_seconds /. (calls1 - calls0 |> float)
        ; cpu_used_percentage= cpu_used_seconds /. sample_interval
        }
    in
    let controller = Controller.update last.controller stats in
    let delay = Controller.get_delay controller in
    let since = Mtime_clock.counter () in
    let (_ : bool) =
      Atomic.compare_and_set t.last last
        {sample; controller; since; stats; delay}
    in
    (* if we raced with another thread (can only happen on OCaml 5), then do nothing and keep their update. *)
    ()

  let stats t =
    let last = Atomic.get t.last in
    (last.stats, last.delay)

  let all_stats () = !all |> List.map (fun (k, v) -> k, stats v)

  let with_limit t f =
    let last = Atomic.get t.last in
    let span = Mtime_clock.count last.since in
    if Mtime.Span.compare span update_interval >= 0 then
      update t span last ;
    (* refresh, might've been updated *)
    let last = Atomic.get t.last in
    if last.delay > Float.epsilon then
      Thread.delay last.delay ;
    Rusage.measure_rusage t.measure f
end
