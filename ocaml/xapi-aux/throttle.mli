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
  (** evaluated on first [execute] *)
end

module Make (_ : SIZE) : sig
  (** [execute f] up to [Size.n ()] in parallel. *)

  val execute : (unit -> 'a) -> 'a
end

module API_cpu_usage : sig
  (** records API usage statistics *)
  type !+'a t

  (** global resource usage statistics, thread safe *)
  type global

  (** local resource limit *)
  type local

  val make : string -> global t
  (** [make name] creates a global throttle for CPU usage of API [name].

    It is safe to store this into a global value and share it between threads.
  *)

  val with_limit : ?max_cpu_usage:float -> ?min_batch_delay:float -> global t -> (local t -> 'a) -> 'a
  (** [with_limit ?max_cpu_usage ?min_batch_delay limit f] calls [f], potentially rate limiting it if it uses too much CPU.

      CPU usage is determined using {!val:Rusage_thread.getrusage_thread_utime} taking only user time into account.
      During (long running) kernel time the OCaml runtime lock is released and other threads can execute.

      [min_batch_delay] can be used to specify a minimum delay for each API call. 
      This is useful for API calls that can batch responses, e.g. [Event.from] and [Event.next], e.g.if multiple fields are updated in a record.

      [max_cpu_usage] is a value between [[0, 1]] that specifies the per-thread CPU usage limit in [5s] intervals.
      When usage is exceeded we begin inserting exponential backoff.       

      The [t] argument of [f] can be used for rate limiting internal recursive/retry loops within [f].
 *)

  val with_recursive : ((local t -> 'a) -> local t -> 'a) -> local t -> 'a
  (** [with_recursive f limit] calls [f self] with a [self] argument that can be used to call [f] recursively.
  A delay is inserted between recursive calls to limit CPU usage and improve batching, and a delay is always inserted upon return.

 *)
end
