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

(** client-side for remote database access protocol v2 *)

open Db_rpc_common_v3
open Db_exn

module D = Debug.Make (struct let name = __MODULE__ end)

module Make =
functor
  (RPC : Db_interface.RPC)
  ->
  struct
    type field = Schema.Value.t

    open Db_interface

    type regular_fields = (field_name * field) list

    type associated_fields = (field_name * row_ref list) list

    (** dictionary of regular fields x dictionary of associated set_ref values *)
    type db_record = regular_fields * associated_fields

    let initialise = RPC.initialise

    let rpc x =
      let reply = RPC.rpc (Wire.to_string x) in
      match reply |> Wire.parse_string with
      | Ok r ->
          r
      | Error (pos, str) ->
          D.error "%s: failed to parse RPC reply at position %d: %s" __LOC__ pos
            str ;
          D.debug "The RPC that we failed to parse was: %S" str ;
          raise Remote_db_server_returned_bad_message

    let process (x : Request.t) response_of_sexp =
      x |> Request.sexp_of_t |> rpc |> response_of_sexp

    let get_table_from_ref _ x =
      let (Response.Get_table_from_ref y) =
        process (Request.Get_table_from_ref x)
          Response.get_table_from_ref_of_sexp
      in
      y

    let is_valid_ref _ x =
      let (Response.Is_valid_ref y) =
        process (Request.Is_valid_ref x) Response.is_valid_ref_of_sexp
      in
      y

    let read_refs _ x =
      let (Response.Read_refs y) =
        process (Request.Read_refs x) Response.read_refs_of_sexp
      in
      y

    let read_field_where _ x =
      let (Response.Read_field_where y) =
        process (Request.Read_field_where x) Response.read_field_where_of_sexp
      in
      y

    let db_get_by_uuid _ t u =
      let (Response.Db_get_by_uuid y) =
        process (Request.Db_get_by_uuid (t, u)) Response.db_get_by_uuid_of_sexp
      in
      y

    let db_get_by_name_label _ t l =
      let (Response.Db_get_by_name_label y) =
        process
          (Request.Db_get_by_name_label (t, l))
          Response.db_get_by_name_label_of_sexp
      in
      y

    let create_row _ x y z =
      let (Response.Create_row y) =
        process (Request.Create_row (x, y, z)) Response.create_row_of_sexp
      in
      y

    let delete_row _ x y =
      let (Response.Delete_row y) =
        process (Request.Delete_row (x, y)) Response.delete_row_of_sexp
      in
      y

    let write_field _ a b c d =
      let (Response.Write_field y) =
        process (Request.Write_field (a, b, c, d)) Response.write_field_of_sexp
      in
      y

    let read_field _ x y z =
      let (Response.Read_field y) =
        process (Request.Read_field (x, y, z)) Response.read_field_of_sexp
      in
      y

    let find_refs_with_filter _ s e =
      let (Response.Find_refs_with_filter y) =
        process
          (Request.Find_refs_with_filter (s, e))
          Response.find_refs_with_filter_of_sexp
      in
      y

    let read_record _ x y =
      let (Response.Read_record (x, y)) =
        process (Request.Read_record (x, y)) Response.read_record_of_sexp
      in
      (x, y)

    let read_records_where _ x e =
      let (Response.Read_records_where y) =
        process
          (Request.Read_records_where (x, e))
          Response.read_records_where_of_sexp
      in
      y

    let process_structured_field _ a b c d e =
      let (Response.Process_structured_field y) =
        process
          (Request.Process_structured_field (a, b, c, d, e))
          Response.process_structured_field_of_sexp
      in
      y
  end
