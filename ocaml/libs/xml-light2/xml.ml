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
(*
 * This is a replacement interface for xml-light that use the superior xmlm
 * engine to parse stuff. Also the output functions SKIP characters that are
 * not allowed in XML.
 *)

(* tree representation *)
type xml = xml Xmlm.frag

type error_pos = {eline: int}

type error = string * error_pos

exception Error of error

let error (msg, pos) = Printf.sprintf "%s line %d" msg pos.eline

let _ =
  let printer = function
    | Error e ->
        Some (Printf.sprintf "XML Error: %s" (error e))
    | _ ->
        None
  in
  Printexc.register_printer printer

(* internal parse function *)
let is_empty xml =
  let is_empty_string s =
    String.for_all (function '\n' | ' ' | '\t' -> true | _ -> false) s
  in
  match xml with `Data data when is_empty_string data -> true | _ -> false

let parse' i =
  let el (tag : Xmlm.tag) (children : xml list) : xml =
    `El (tag, List.filter (fun xml -> not (is_empty xml)) children)
  in
  let data s = `Data s in
  match Xmlm.peek i with
  | `Dtd _ ->
      snd (Xmlm.input_doc_tree ~el ~data i)
  | _ ->
      Xmlm.input_tree ~el ~data i

let parse i =
  try parse' i
  with Xmlm.Error ((line, _), msg) ->
    let pos = {eline= line} in
    let err = Xmlm.error_message msg in
    raise (Error (err, pos))

(* common parse function *)
let parse_file file =
  In_channel.with_open_text file @@ fun chan ->
  let i = Xmlm.make_input (`Channel chan) in
  parse i

let parse_in chan =
  let i = Xmlm.make_input (`Channel chan) in
  parse i

let parse_string s =
  let i = Xmlm.make_input (`String (0, s)) in
  parse i

let to_string xml =
  let buffer = Buffer.create 1024 in
  let dest = Xmlm.make_output ~decl:false (`Buffer buffer) in
  Xmlm.output_doc_tree Fun.id dest (None, xml);
  Buffer.contents buffer

let to_string_fmt xml =
  let buffer = Buffer.create 1024 in
  let dest = Xmlm.make_output ~decl:false ~indent:(Some 2) (`Buffer buffer) in
  Xmlm.output_doc_tree Fun.id dest (None, xml);
  Buffer.contents buffer

let element tag attrs children = `El ((("", tag), attrs), children)

let pcdata str = `Data str

let value_of_attrs_opt key attrs =
  List.find_map
    (fun ((_, k), v) -> if String.equal key k then Some v else None)
    attrs

let value_of_attrs_exn key attrs = value_of_attrs_opt key attrs |> Option.get
