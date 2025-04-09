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

(** tree representation *)
type xml = xml Xmlm.frag

type error_pos

type error = string * error_pos

exception Error of error

val error : error -> string

val parse_file : string -> xml
(** input functions *)

val parse_in : in_channel -> xml

val parse_string : string -> xml

val to_string : xml -> string

val to_string_fmt : xml -> string

val element : string -> (string * string) list -> xml list -> xml

val pcdata : string -> xml

val value_of_attrs_exn: string -> Xmlm.attribute list -> string
val value_of_attrs_opt: string -> Xmlm.attribute list -> string option
