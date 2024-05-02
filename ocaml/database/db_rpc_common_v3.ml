(*
 * Copyright (C) 2010 Citrix Systems Inc.
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

(** Marshall/unmarshall functions and types for db remote access protocol v3 *)

open Sexplib0.Sexp_conv
open Db_interface

module Wire = Csexp.Make(Sexplib0.Sexp)

module Request = struct
  type t =
    | Get_table_from_ref of row_ref
    | Is_valid_ref of row_ref
    | Read_refs of table
    | Find_refs_with_filter of table * Db_filter_types.expr
    | Read_field_where of Db_cache_types.where_record
    | Db_get_by_uuid of table * uuid
    | Db_get_by_name_label of table * string
    | Create_row of table * (string * Schema.Value.t) list * row_ref
    | Delete_row of table * row_ref
    | Write_field of table * row_ref * field_name * Schema.Value.t
    | Read_field of string * table * row_ref
    | Read_record of table * row_ref
    | Read_records_where of table * Db_filter_types.expr
    | Process_structured_field of
        (string * string)
        * table
        * field_name
        * row_ref
        * Db_cache_types.structured_op_t
  [@@deriving sexp_of]

  (* Make sure the slave only ever uses the idempotent version *)
  let sexp_of_t t =
    let t' =
      match t with
      | Process_structured_field (a, b, c, d, Db_cache_types.AddMapLegacy) ->
          Process_structured_field (a, b, c, d, Db_cache_types.AddMap)
      | x ->
          x
    in
    sexp_of_t t'
end

module Response = struct
  (* TODO:wrap exceptions as variants... *)
  type 'a t = ('a, exn) result [@@deriving of_sexp]
  
  type get_table_from_ref = Get_table_from_ref of table option
  [@@deriving of_sexp]

  type is_valid_ref = Is_valid_ref of bool [@@deriving of_sexp]

  type read_refs = Read_refs of row_ref list [@@deriving of_sexp]

  type find_refs_with_filter = Find_refs_with_filter of row_ref list
  [@@deriving of_sexp]

  type read_field_where = Read_field_where of Schema.Value.t list
  [@@deriving of_sexp]

  type db_get_by_uuid = Db_get_by_uuid of row_ref [@@deriving of_sexp]

  type db_get_by_name_label = Db_get_by_name_label of row_ref list
  [@@deriving of_sexp]

  type create_row = Create_row of unit [@@deriving of_sexp]

  type delete_row = Delete_row of unit [@@deriving of_sexp]

  type write_field = Write_field of unit [@@deriving of_sexp]

  type read_field = Read_field of Schema.Value.t [@@deriving of_sexp]

  type read_record =
    | Read_record of
        (field_name * Schema.Value.t) list * (field_name * row_ref list) list
  [@@deriving of_sexp]

  type read_records_where =
    | Read_records_where of
        ( row_ref
        * ((field_name * Schema.Value.t) list * (field_name * row_ref list) list)
        )
        list
  [@@deriving of_sexp]

  type process_structured_field = Process_structured_field of unit
  [@@deriving of_sexp]
end
