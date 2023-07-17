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

(** Zero-copy buffers *)

(** A producer/consumers buffer that does not allocate once created *)
type t

val src: Logs.src
(** [src] is this module's logging source. Can be used to set logging level. *)

val of_bigstring : Bigstringaf.t -> off:int -> len:int -> t
(** [of_bigstring bigstring ~off ~len] is a buffer using the region [[off, off+len)] of [bigstring].
 The buffer can be uninitialized. *)

(** read-only *)
type ro

(** write-only *)
type wo

module View : sig
  (** a view of the underlying buffer *)
  type 'a t

  val get: ro t -> int -> char
  (** [get t pos] retrieves the character at [pos] in [t]. *)

  val memchr : ro t -> pos:int -> char -> int
  (** [memchr view ~pos char] searches for [char] in [view].
     [pos] is relative to the beginning of [view].
     Returns [-1] if the [char] is not found, or an offset relative to [view] otherwise.
  *)

  val is_prefix: ro t -> string -> bool
  (** [is_prefix view str] compares the prefix of [view] with [str]. *)

  val size : _ t -> int
  (** [size view] is the size in bytes of [view]. *)

  val debug: ro t -> len:int -> unit
  (** [debug t len] prints buffer up to [len] for debugging. *)
end

val producer : t -> wo View.t
(** [producer t] is a write-only view of the underlying buffer. *)

type 'a refill = 'a -> off:int -> len:int -> Bigstringaf.t -> int

val refill: t -> 'a refill -> 'a -> unit
(** [refill t reader input] calls [reader input ~off ~len] to refill the internal buffer.
 [reader] must not touch the buffer outside of [[off, len)], and it must return the amount of bytes written
 into the buffer.
 This will advance the internal produced.
*)

val consumer : t -> ro View.t
(** [consumer t] is a read-only view of the underlying buffer. *)

val consumed : t -> int -> unit
(** [consumed t amount] moves the consumer offset by [amount]. *)

val check_invariant: t -> unit
(** [check_invariant t] checks [t]'s invariants. *)

val reset: t -> unit
(** [reset t] resets all consumer/producer offsets. *)

val is_eof: t -> bool
(** [is_eof t] checks whether EOF has been reached on the input, and all data in the buffer has been consumed. *)