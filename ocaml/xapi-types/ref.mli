(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type 'a t

val ref_prefix : string

val make : unit -> 'a t

val null : 'a t

val compare : 'a t -> 'a t -> int
(** [compare a b] returns [0] if [a] and [b] are equal, a negative integer if
    [a] is less than [b], and a positive integer if [a] is greater than [b]. *)

val string_of : 'a t -> string

val to_option : 'a t -> 'a t option
(** [to_option ref] returns [None] when [ref] is [Ref.Null] or [Some ref]
    otherwise *)

val short_string_of : 'a t -> string

val of_string : string -> 'a t

val make_dummy : string -> 'a t

val is_real : 'a t -> bool

val is_dummy : 'a t -> bool

val name_of_dummy : 'a t -> string

val really_pretty_and_small : 'a t -> string

val pp : Format.formatter -> 'a t -> unit
