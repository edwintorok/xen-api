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
    type range = {min:int ; max: int}
    type t =
      | UnboxedInt of range (** integers between [min] and [max], inclusive *)
      | Block of { tag: int; size: range; elements: t array option }

    let fixed n = {min=n;max=n}
    let string_size = {min = 1; max = 1 + Sys.max_string_length * 8 / Sys.word_size}

    let bool = UnboxedInt {min=0; max = 1}
    let char = UnboxedInt {min = 0; max = 255}
    let int = UnboxedInt {min = min_int; max = max_int}
    let block tag ?elements size = Block {tag; size; elements }
    let float = block Obj.double_tag (fixed 1)
    let bytes = block Obj.string_tag string_size
    let string = block Obj.string_tag string_size
    let int32 = block Obj.custom_tag (fixed 1)
    let int64 = block Obj.custom_tag (fixed @@ 64 / Sys.word_size)

    let tuple n lst = block 0 (fixed @@ List.length lst) ~elements:(Array.of_list lst)
    (* TODO: also boxed record field... we really need to call the compiler to figure this out? *)
end

type native_arg =
  | Value
  | Double
  | Int32
  | Int64
  | Intnat of { untagged_int: bool }
  | Bytecode_argv
  | Bytecode_argn

let native_arg_of_primitive =
  let open Primitive in
  function
  | Same_as_ocaml_repr ->
      Value
  | Unboxed_float ->
      Double
  | Unboxed_integer Pnativeint ->
      Intnat { untagged_int = false }
  | Unboxed_integer Pint32 ->
      Int32
  | Unboxed_integer Pint64 ->
      Int64
  | Untagged_int ->
      (* the range of this is one bit less than Pnativeint, but still same type on C side *)
      Intnat { untagged_int = true }

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
  let primitive_description pd =
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
  let all = ref [] in
  let rec value_description _ vd =
    let open Typedtree in
    let open Types in
    match vd.val_val.val_kind with
    | Val_prim prim ->
        let env = vd.val_desc.ctyp_env in
        Env.fold_constructors (fun constr () ->
            match constr.cstr_tag with
            | Cstr_constant n -> Printf.eprintf "%d\n" n
            | Cstr_block n -> Printf.eprintf "%d\n" n
            | Cstr_unboxed -> Printf.eprintf "unboxed\n"
            | Cstr_extension _ -> Printf.eprintf "ext\n"

        ) None env ();
        List.iter (fun cid ->
            try let constr = Env.find_ident_constructor cid env in
            match constr.cstr_tag with
            | Cstr_constant n -> Printf.eprintf "%d\n" n
            | Cstr_block n -> Printf.eprintf "%d\n" n
            | Cstr_unboxed -> Printf.eprintf "unboxed\n"
            | Cstr_extension _ -> Printf.eprintf "ext\n"
            with Not_found -> ()
        ) !all;
        primitive_description prim
    | _ ->
        ()
  in
  let type_kind _ tkind =
    let open Typedtree in
    let open Types in
    match tkind with
    | Ttype_abstract -> ()
    | Ttype_record _ -> () (* TODO *)
    | Ttype_variant cnstr ->
        Printf.eprintf "Got %d constructors" (List.length cnstr);
        List.iter (fun c ->
            all := c.Typedtree.cd_id :: !all 
        ) cnstr;
        ()
    | Ttype_open -> ()
  in
  let open Tast_iterator in
  let iterator = {default_iterator with value_description; type_kind} in
  path
  |> Cmt_format.read_cmt
  |>
  let open Cmt_format in
  function
  | {cmt_annots= Implementation structure;} ->
        Env.fold_constructors (fun constr () ->
            match constr.cstr_tag with
            | Cstr_constant n -> Printf.eprintf "%d\n" n
            | Cstr_block n -> Printf.eprintf "%d\n" n
            | Cstr_unboxed -> Printf.eprintf "unboxed\n"
            | Cstr_extension _ -> Printf.eprintf "ext\n"

        ) None structure.str_final_env ();
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
