(*
 * Copyright (C) 2010-2011 Citrix Systems Inc.
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
module Sexp = Sexplib0.Sexp
open Sexplib0.Sexp_conv

module Type = struct
  type t = String | Set  (** of strings *) | Pairs  (** of strings *)
  [@@deriving sexp_of]

  exception Error of t * t

  let _ =
    Printexc.register_printer (function
      | Error (expected, actual) ->
          Some
            (Printf.sprintf "Schema.Type.Error: expected %s; received %s"
               (Sexp.to_string_hum (sexp_of_t expected))
               (Sexp.to_string_hum (sexp_of_t actual))
            )
      | _ ->
          None
      )
end

module Value = struct
  module M = Map.Make (String)

  type pairs = string M.t

  type compat_pairs = (string * string) list [@@deriving sexp_of]

  let sexp_of_pairs pairs = pairs |> M.bindings |> sexp_of_compat_pairs

  type t = String of string | Set of string list | Pairs of pairs
  [@@deriving sexp_of]

  let ensure_utf8_xml = function
    | String s as orig ->
      let s' = String_marshall_helper.ensure_utf8_xml s in
      (* use physical equality to avoid allocating when unchanged *)
      if s == s' then orig
      else String s'
    | Set lst as orig ->
      let lst' = List.map String_marshall_helper.ensure_utf8_xml lst in
      if List.for_all2 (==) lst lst' then orig
      else Set lst'
    | Pairs m as orig ->
      let m' = M.mapi (fun k v ->
        if String_marshall_helper.ensure_utf8_xml k == k then
          String_marshall_helper.ensure_utf8_xml v
        else
          (* the compat API still supports it and will truncate as needed *)
          failwith (Printf.sprintf "Non-UTF8 key is not supported anymore: %S" k)
      ) m in
      if m == m' then orig
      else Pairs m'

  let marshal = function
    | String x ->
        x
    | Set xs ->
        String_marshall_helper.set (fun x -> x) xs
    | Pairs xs ->
        String_marshall_helper.map (fun x -> x) (fun x -> x) (M.to_seq xs)

  let unmarshal ty x =
    match ty with
    | Type.String ->
        String x
    | Type.Set ->
        Set (String_unmarshall_helper.set (fun x -> x) x)
    | Type.Pairs ->
        Pairs
          (String_unmarshall_helper.map (fun x -> x) (fun x -> x) x |> M.of_seq)

  module Unsafe_cast = struct
    let string = function
      | String x ->
          x
      | Set _ ->
          raise (Type.Error (Type.String, Type.Set))
      | Pairs _ ->
          raise (Type.Error (Type.String, Type.Pairs))

    let set = function
      | Set xs ->
          xs
      | String _ ->
          raise (Type.Error (Type.Set, Type.String))
      | Pairs _ ->
          raise (Type.Error (Type.Set, Type.Pairs))

    let pairs = function
      | Pairs x ->
          x
      | String _ ->
          raise (Type.Error (Type.Pairs, Type.String))
      | Set _ ->
          raise (Type.Error (Type.Pairs, Type.Set))
  end
end

module Column = struct
  type t = {
      name: string
    ; persistent: bool  (** see is_field_persistent *)
    ; empty: Value.t  (** fresh value used when loading non-persistent fields *)
    ; default: Value.t option
          (** if column is missing, this is default value is used *)
    ; ty: Type.t  (** the type of the value in the column *)
    ; issetref: bool
          (** only so we can special case set refs in the interface *)
  }
  [@@deriving sexp_of]
end

module ColumnMap = struct
  module M = Map.Make (String)

  type t = Column.t M.t * Column.t list

  type compat = Column.t list [@@deriving sexp_of]

  let of_list columns =
    ( columns |> List.fold_left (fun acc c -> M.add c.Column.name c acc) M.empty
    , columns
    )

  let to_list (_, lst) = lst

  let sexp_of_t (_, compat) = sexp_of_compat compat

  let find_opt name (t, _) = M.find_opt name t
end

module Table = struct
  type t = {name: string; columns: ColumnMap.t; persistent: bool}
  [@@deriving sexp_of]

  let find name t =
    match ColumnMap.find_opt name t.columns with
    | Some result ->
        result
    | None ->
        raise (Db_exn.DBCache_NotFound ("missing column", t.name, name))
end

type relationship = OneToMany of string * string * string * string
[@@deriving sexp_of]

module Database = struct
  type t = {tables: Table.t list} [@@deriving sexp_of]

  let find name t =
    try List.find (fun tbl -> tbl.Table.name = name) t.tables
    with Not_found ->
      raise (Db_exn.DBCache_NotFound ("missing table", name, ""))
end

(** indexed by table name, a list of (this field, foreign table, foreign field) *)
type foreign = (string * string * string) list [@@deriving sexp_of]

module ForeignMap = struct
  include Map.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

  type t' = (string * foreign) list [@@deriving sexp_of]

  type m = foreign t

  let sexp_of_m t : Sexp.t =
    let t' = fold (fun key foreign acc -> (key, foreign) :: acc) t [] in
    sexp_of_t' t'
end

type t = {
    major_vsn: int
  ; minor_vsn: int
  ; database: Database.t
        (** indexed by table name, a list of (this field, foreign table, foreign field) *)
  ; one_to_many: ForeignMap.m
  ; many_to_many: ForeignMap.m
}
[@@deriving sexp_of]

let database x = x.database

let table tblname x = Database.find tblname (database x)

let empty =
  {
    major_vsn= 0
  ; minor_vsn= 0
  ; database= {Database.tables= []}
  ; one_to_many= ForeignMap.empty
  ; many_to_many= ForeignMap.empty
  }

let is_table_persistent schema tblname = (table tblname schema).Table.persistent

let is_field_persistent schema tblname fldname =
  let tbl = table tblname schema in
  let col = Table.find fldname tbl in
  tbl.Table.persistent && col.Column.persistent

let table_names schema =
  List.map (fun t -> t.Table.name) (database schema).Database.tables

let one_to_many tblname schema =
  (* If there is no entry in the map it means that the table has no one-to-many relationships *)
  try ForeignMap.find tblname schema.one_to_many with Not_found -> []

let many_to_many tblname schema =
  (* If there is no entry in the map it means that the table has no many-to-many relationships *)
  try ForeignMap.find tblname schema.many_to_many with Not_found -> []
