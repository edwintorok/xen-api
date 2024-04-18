(*
 * Copyright (C) 2024 Cloud Software Group
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

(* similar to [gettimeofday]: doesn't allocate, and can return a float directly without boxing it.
   This avoids introducing extra overhead in a function used to measure overhead.
*)

external getrusage_thread : unit -> (float[@unboxed])
  = "caml_getrusage_thread" "caml_getrusage_thread_unboxed"
[@@noalloc]
(** [getrusage_thread] returns the CPU usage of the current thread, as reported by [clock_gettime(CLOCK_THREAD_CPUTIME_ID)].
 This is more accurate than the (Linux specific) [getrusage(RUSAGE_THREAD)], which only reports values with millisecond precision.
 *)
