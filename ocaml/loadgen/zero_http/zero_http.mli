(*
 * Copyright (C) 2023 Cloud Software Group
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

(**
  An HTTP response parser for benchmarking/load generation purposes that performs 0 allocations.
  This avoids unpredictable GC latencies from affecting the benchmarks/measurements.

  It preallocates all HTTP requests and optimistically assumes that responses will be 200 OK.
  It will allocate memory only when not on that fastpath.
*)

module Zero_buffer : sig
  include module type of Zero_buffer
end

module Response : sig
  type t

  val create : Zero_buffer.t -> ('a Zero_buffer.refill) -> 'a -> (status_code:int -> content_length:int -> headers_size:int -> unit) -> t
  (** [create buff callback] creates new HTTP response parser state that invokes [callback] when a response has been parsed. *)

  val read: t -> unit
  (** [read t ] reads data using [reader] and parses a potentially partial HTTP response stream.
    This is a fastpath if the following conditions are met:
      * the response is <HTTP status code 4xx
      * a Content-Length header is present with that exact capitalization
      * the body and headers are not required and will be discarded

    If these conditions are not met then the code falls back to slower parsing that allocates memory.

    @param t stores parsers state for a response
    @param reader is a function to read data from some connection ['a] into the supplied buffer and return the amount of data read
    @param conn is a parameter for [reader] (e.g. a file descriptor)
  *)
end