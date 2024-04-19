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

module Batching : sig
  val with_recursive :
       delay_before:(unit -> float)
    -> delay_after:(unit -> float)
    -> (('a -> 'b) -> 'a -> 'b)
    -> 'a
    -> 'b
  (** [with_recursive ~delay_before ~delay_after f arg] calls [f self arg], where [self] can be used
    for recursive calls.
    A [delay_before ()] amount of seconds is inserted before each call to [f], and [delay_after ()] after.
   *)
end

module Rusage : sig
  (** store CPU resource usage measurements *)
  type t

  val make : string -> t
  (** [make name] creates thread-safe storage for measuring resource usage for [name]. *)

  val measure_rusage : t -> (unit -> 'a) -> 'a
  (** [measure_rusage t f] measures the CPU resource usage of [f ()], updating [t] with the measurements.
    CPU resource usage is measured using {!Rusage_thread.getrusage_thread}.
    This function is thread-safe.
   *)

  val sample : t -> float
  (** [sample t] is the cumulative resource usage for [t] in seconds. *)
end
