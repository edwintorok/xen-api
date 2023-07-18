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

(** Zero-copy line parsing *)

(** a parser that looks for \r\n *)
type t

val make: Zero_buffer.t -> read:'a Zero_buffer.refill -> 'a -> t
(** [make zbuf ~read input] creates a line parser that uses [read input] to read data and [zbuf] as temporary storage. *)

val read_line: t -> ('a -> 'b -> Zero_buffer.ro Zero_buffer.View.t -> eol_len:int -> 'a) -> 'a -> 'b -> 'a
(** [read_line t process_line acc input] calls [process_line acc input] with a buffer containing a single line to be parsed,
 and the accumulator [acc]. *)

val read_data: t -> ('a -> Zero_buffer.ro Zero_buffer.View.t -> int) -> 'a -> bool
(** [read_data t callback] invokes [callback] when there is data available to read. *)

val is_bol : t -> bool
(** [is_bol t] whether [t] is at the beginning of line. *)