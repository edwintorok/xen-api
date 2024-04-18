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

module D = Debug.Make (struct let name = __MODULE__ end)

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

module API_cpu_usage = struct
  type !+'a t = 'a

  type sample = {timestamp: float; sampled_requests: int; sampled_us: int}

  type global = {
      name: string
    ; us: int Atomic.t  (** user time in microseconds *)
    ; requests: int Atomic.t
    ; last_sample: sample Atomic.t
    ; delay: float Atomic.t
  }

  type local = float

  let make name =
    {
      name
    ; requests= Atomic.make 0
    ; us= Atomic.make 0
    ; last_sample=
        Atomic.make
          {sampled_us= 0; sampled_requests= 0; timestamp= Unix.gettimeofday ()}
    ; delay= Atomic.make 0.
    }

  let sample_interval = 5.

  let sample ~min_batch_delay ~max_cpu_usage global ~timestamp ~sampled_requests
      ~sampled_us =
    let last_sample =
      Atomic.exchange global.last_sample
        {timestamp; sampled_requests; sampled_us}
    in
    let delta_t = timestamp -. last_sample.timestamp
    and delta_utime = float (sampled_us - last_sample.sampled_us) *. 1e-6
    and delta_requests =
      float (sampled_requests - last_sample.sampled_requests)
    in
    (* TODO: remove *)
    D.debug "delta_t: %g, delta_utime: %g, delta_requets: %g " delta_t
      delta_utime delta_requests ;
    (* we could've raced with another update, or clock was adjusted *)
    if delta_t > 0. && delta_utime > 0. && delta_requests > 0. then (
      let avg_utime_per_request = delta_utime /. delta_requests in
      let avg_elapsed_per_request = avg_utime_per_request +. min_batch_delay in
      let max_requests =
        Float.max delta_requests (delta_t /. avg_elapsed_per_request)
      in
      let delay =
        (Float.max 0. (1. -. max_cpu_usage) *. delta_t /. max_requests)
        -. avg_elapsed_per_request
      in
      let old_delay = Atomic.exchange global.delay delay in
      D.debug "delay: %g" delay ;
      if abs_float (old_delay -. delay) >= 0.1 then
        D.debug "API call delay changed %.2f -> %.2f" old_delay delay
    )

  let with_limit ?(max_cpu_usage = 0.25) ?(min_batch_delay = 0.05) global f =
    (* we assume that each thread is dedicated to one API call, but the thread may be reused
       (e.g. if a thread pool is used to handle API calls), so we only measure CPU usage
       inside this function.
    *)
    let utime0 = Rusage_thread.getrusage_thread_utime () in
    let delay = Atomic.get global.delay in
    if delay > 0. then
      Thread.delay delay ;
    let finally () =
      let current = Unix.gettimeofday () in
      let last_sample = Atomic.get global.last_sample in
      let delta_us =
        (Rusage_thread.getrusage_thread_utime () -. utime0) *. 1e6
        |> Float.ceil
        |> int_of_float
      in
      let sampled_us = Atomic.fetch_and_add global.us delta_us
      and sampled_requests = Atomic.fetch_and_add global.requests 1 in
      (* TODO: remove *)
      D.debug "delta %g" (current -. last_sample.timestamp) ;
      if current -. last_sample.timestamp >= sample_interval then
        sample global ~min_batch_delay ~max_cpu_usage ~timestamp:current
          ~sampled_requests ~sampled_us
    in
    Fun.protect ~finally (fun () -> f min_batch_delay)

  let with_recursive f =
    let rec self delay =
      if delay > 0. then
        Thread.delay delay ;
      (f [@tailcall]) self delay
    in
    self
end
