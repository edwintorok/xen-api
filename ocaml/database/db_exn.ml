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
open Sexplib0.Sexp_conv

(** class * field * uuid * key *)
exception Duplicate_key of string * string * string * string
[@@deriving sexp]

(** message * class * key *)
exception DBCache_NotFound of string * string * string
[@@deriving sexp]

(** class * field * key *)
exception Uniqueness_constraint_violation of string * string * string
[@@deriving sexp]

(** class * field * value *)
exception Integrity_violation of string * string * string
[@@deriving sexp]

(** class * _ * uuid *)
exception Read_missing_uuid of string * string * string
[@@deriving sexp]

(** class * _ * uuid *)
exception Too_many_values of string * string * string
[@@deriving sexp]

exception Remote_db_server_returned_unknown_exception
[@@deriving sexp]

exception Remote_db_server_returned_bad_message
[@@deriving sexp]

exception Empty_key_in_map
[@@deriving sexp]

exception Invalid_value
[@@deriving sexp]
