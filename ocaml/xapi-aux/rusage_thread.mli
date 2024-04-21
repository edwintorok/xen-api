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

external getrusage_thread_ns : unit -> (int64[@unboxed])
  = "caml_getrusage_thread_ns" "caml_getrusage_thread_ns_unboxed"
[@@noalloc]
(** [getrusage_thread_ns] returns the CPU time consumed by the current thread in nanoseconds.

 The time includes the time that the thread was busy executing code, but does not include the time where the thread was idle (e.g. waiting for a lock, or sleeping).

 The cpu time is currently measured by [clock_gettime(CLOCK_THREAD_CPUTIME_ID)], which has 1 microsecond or better accuracy on Linux.
 This is more accurate than the (Linux specific) [getrusage(RUSAGE_THREAD)], which only reports values with millisecond precision.

 The function does not allocate, and returns an unboxed int64 on native builds.
 Bytecode will box and allocate, this is similar to how {!val:Unix.gettimeofday} is implemented,
 except with [int64] as a result.

 To avoid allocation the returned [int64] should be used immediately (e.g. converted to [int]).
 Converting to [int] on 64-bit systems won't overflow for >100years uptime.
*)
