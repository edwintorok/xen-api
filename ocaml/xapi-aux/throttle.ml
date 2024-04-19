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
  }

  let make name = {name; ns= Atomic.make 0}

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
      ()
    in
    Fun.protect ~finally f

  let sample t = float (Atomic.get t.ns) *. 1e-9
end
