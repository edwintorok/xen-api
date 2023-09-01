(*
 * Copyright (C) Cloud Software Group, Inc.
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

(** [Typedtree] and [Primitive] have an unstable API (depends on compiler version),
    so extract the parts we need and convert to types defined in this file.
    If the build breaks with new compiler versions then only this module needs
    to be updated (perhaps by using Dune's support to conditionally select
    files based on compiler versions)
*)

module Shape = struct
  (** https://v2.ocaml.org/manual/intfc.html#s%3Ac-ocaml-datatype-repr *)
  type range = {
      min: int  (** minimum possible value for type *)
    ; max: int  (** maximum possible value for type *)
  }
  (** range of an integer. Needed because [unit], [bool], [char], [int] and simple variants are all
    represented as integers when interfacing with C, but for value analysis it is useful to know
    their range.
   *)

  (** Integers and float array elements are stored directly in values *)
  type unboxed =
    | OCamlInt of range  (** OCaml integer with given range *)
    | DoubleArrayElement  (** An unboxed float array element *)

  (** information about the size and layout of an OCaml type *)
  type t =
    | Unboxed of unboxed  (** directly stored in a value *)
    | Boxed of boxed  (** pointer stored in value, allocated separately *)
    | Exception  (** exceptions have dedicated API calls *)
    | Unknown  (** a value we cannot yet analyze (e.g. abstract type) *)
    | Variant of unboxed option * boxed array
        (** a variant can contain both boxed and unboxed types: [A | B of ... | C ...] *)

  and boxed =
    | Double  (** OCaml [float] *)
    | Int32  (** can be smaller than a word, special case *)
    | IntN of {words: int}  (** [int64], [nativeint] *)
    | String of {writable: bool  (** [string] is not writable *)}
        (** [string] or [bytes] *)
    | Tuple of t array  (** (e1,...,en) each element can have different shape *)
    | Array of {elements: t}
        (** [| element; ... |], all elements have same shape *)
    | Block of {tag: int; elements: t array}
        (** {field1: ...; ...; fieldN: ...} *)
    | Object
  (*  | Self of {levels:int} *)

  let fixed n = {min= n; max= n}

  let string_size =
    {min= 0; max= 1 + (Sys.max_string_length * 8 / Sys.word_size)}

  let unit = Unboxed (OCamlInt {min= 0; max= 0})

  let bool = Unboxed (OCamlInt {min= 0; max= 1})

  let char = Unboxed (OCamlInt {min= 0; max= 255})

  let int = Unboxed (OCamlInt {min= min_int; max= max_int})

  let constructor x = Unboxed (OCamlInt {min= x; max= x})

  let block tag elements = Boxed (Block {tag; elements})

  let bytes = Boxed (String {writable= true})

  let string = Boxed (String {writable= false})

  let float = Boxed Double

  let int32 = Boxed Int32

  let int64 = Boxed (IntN {words= 64 / Sys.word_size})

  let nativeint = Boxed (IntN {words= 1})

  let tuple lst = Boxed (Tuple (Array.of_list lst))

  let is_double = function Boxed Double -> true | _ -> false

  let exn = Exception

  let record lst =
    if List.for_all is_double lst then
      (* TODO: depends on compiler version/flags? *)
      Boxed (Array {elements= Unboxed DoubleArrayElement})
    else
      tuple lst

  let array elements = Boxed (Array {elements})

  (* TODO: depends on compiler version/flags? *)
  let floatarray = Boxed (Array {elements= Unboxed DoubleArrayElement})

  let obj = Boxed Object

  let is = Types.eq_type

  let rec of_type_expr e =
    let open Predef in
    if is e type_int then
      int
    else if is e type_char then
      char
    else if is e type_string then
      string
    else if is e type_bytes then
      bytes
    else if is e type_float then
      float
    else if is e type_bool then
      bool
    else if is e type_unit then
      unit
    else if is e type_nativeint then
      nativeint
    else if is e type_int32 then
      int32
    else if is e type_int64 then
      int64
    else if is e type_exn then
      exn
    else if is e type_floatarray then
      floatarray
    else
      match Types.get_desc e with
      | Ttuple lst ->
          tuple (List.map of_type_expr lst)
      | Tobject _ ->
          obj
      | Tvar _ | Tarrow _ | Tconstr _ | Tfield _ | Tnil | Tunivar _ | Tpackage _
        ->
          Unknown
      | Tlink e | Tsubst (e, _) | Tpoly (e, _) ->
          (* TODO: substitute type variables in call... *)
          of_type_expr e
      | Tvariant _ ->
          Unknown (* TODO: use constructor_description here *)
end

type native_arg =
  | Value of Shape.t
  | Double
  | Int32
  | Int64
  | Intnat of {untagged_int: bool}
  | Bytecode_argv
  | Bytecode_argn

let native_arg_of_primitive type_expr =
  let open Primitive in
  function
  | Same_as_ocaml_repr ->
      Value (Shape.of_type_expr type_expr)
  | Unboxed_float ->
      Double
  | Unboxed_integer Pnativeint ->
      Intnat {untagged_int= false}
  | Unboxed_integer Pint32 ->
      Int32
  | Unboxed_integer Pint64 ->
      Int64
  | Untagged_int ->
      (* the range of this is one bit less than Pnativeint, but still same type on C side *)
      Intnat {untagged_int= true}

(**  [ctype_of_native_arg arg] returns the C type used when implementing
     primitives for native code mode.

    @see <https://v2.ocaml.org/manual/intfc.html#ss:c-unboxed> on the use of [intnat]*)
let ctype_of_native_arg = function
  | Value ->
      "value"
  | Double ->
      "double"
  | Int32 ->
      "int32_t"
  | Int64 ->
      "int64_t"
  | Intnat _ ->
      "intnat"
  | Bytecode_argv ->
      "value *"
  | Bytecode_argn ->
      "int"

type t = {
    byte_name: string
        (** name of C function implementing the primitive in bytecode mode *)
  ; native_name: string
        (** name of C function implementinmg the primitive in native code mode *)
  ; arity: int  (** number of arguments to C function in native code mode *)
  ; alloc: bool  (** whether it allocates/raises exceptions *)
  ; native_result: native_arg
        (** result type of the C function implementing the primitive in native code mode*)
  ; native_args: native_arg list
        (** type of the arguments of the C function implementing the primitive in native code mode *)
}

(** [with_report_exceptions f] will report any compiler-libs exceptions
    escaping from [f] and exit the process with code 2. *)
let with_report_exceptions f =
  try f ()
  with e ->
    (* if there are any errors loading or processing the .cmt file,
       or other exceptions escaping from compiler-libs this will report them properly *)
    Location.report_exception Format.err_formatter e ;
    exit 2

(** [warning loc fmt] prints a warning at source location [loc],
    with message format defined by [fmt].

    This will issue a warning 22 (preprocessor).
 *)
let warning loc =
  Printf.ksprintf @@ fun msg -> Location.prerr_warning loc (Preprocessor msg)

(** [iter_primitives_exn ~path primitive_description] will load the .cmt/.cmti file
 [path] and iterate on any primitives defined using [primitive_description].

  Exceptions from compiler-libs may escape, so it is recommended to wrap calls
  using [with_report_exceptions].
 *)
let iter_primitives_exn ~path f =
  let primitive_description type_expr pd =
    (* TODO: get a Tarrow list from type_expr, split and zip over... *)
    let open Primitive in
    if native_name_is_external pd then
      (* only process primitives implemented by the user, not the ones defined
         by the compiler itself *)
      let t =
        {
          byte_name= byte_name pd
        ; native_name= native_name pd
        ; arity= pd.prim_arity
        ; native_result= native_arg_of_primitive pd.prim_native_repr_res
        ; alloc= pd.prim_alloc
        ; native_args= List.map native_arg_of_primitive pd.prim_native_repr_args
        }
      in
      f t
  in
  let rec value_description _ vd =
    let open Typedtree in
    let open Types in
    match vd.val_val.val_kind with
    | Val_prim prim ->
        primitive_description vd.val_val.val_type prim
    | _ ->
        ()
  in
  let type_kind _ tkind =
    let open Typedtree in
    match tkind with
    | Ttype_abstract ->
        ()
    | Ttype_record _ ->
        () (* TODO *)
    | Ttype_variant cnstr ->
        Printf.eprintf "Got %d constructors" (List.length cnstr) ;
        ()
    | Ttype_open ->
        ()
  in
  let open Tast_iterator in
  let iterator = {default_iterator with value_description; type_kind} in
  path
  |> Cmt_format.read_cmt
  |>
  let open Cmt_format in
  function
  | {cmt_annots= Implementation structure; _} ->
      iterator.structure iterator structure
  | {cmt_annots= Interface signature; _} ->
      (* this won't find all primitives, because the interface is allowed to
         hide the implementation detail by using 'val ...' instead of 'external ...'
      *)
      warning (Location.in_file path)
        "Loaded a .cmti file. May not contain all primitives" ;
      iterator.signature iterator signature
  | _ ->
      invalid_arg
        "Could not find an implementation or interface in the .cmt/.cmti file"
