(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

let my_name = Filename.basename Sys.argv.(0)

type frame = {
    process: string
  ; filename: string
  ; line: int
  ; characters: (int * int) option [@sexp.option]
  ; line_end: int option [@sexp.option]
  ; name: string option [@sexp.option]
  ; is_inline: bool option [@sexp.option]
  ; is_raise: bool option [@sexp.option]
}
[@@deriving sexp, rpcty]

type t = frame list [@@deriving sexp, rpcty]

let empty = []

let[@inline always] try_with f handler =
  try f ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    handler e bt

let to_string_hum xs =
  let xs' = List.length xs in
  let results = Buffer.create 10 in
  let rec loop first_line i = function
    | [] ->
        Buffer.contents results
    | x :: xs ->
        let raise_or_call =
          if first_line then
            "Raised at"
          else if x.is_raise = Some true then
            "Re-raised at"
          else
            "Called from"
        and inlined = if x.is_inline = Some true then " (inlined)" else "" in
        Printf.bprintf results "%d/%d %s %s file \"%s\"%s, line %d" i xs'
          x.process raise_or_call x.filename inlined x.line ;
        x.characters
        |> Option.iter (fun (s, e) ->
               Printf.bprintf results ", characters %d-%d" s e
           ) ;
        Buffer.add_string results "\n" ;
        loop false (i + 1) xs
  in
  loop true 1 xs

let frame_of_slot process slot =
  let open Printexc in
  match Slot.location slot with
  | None ->
      None
  | Some loc ->
      Some
        {
          process
        ; filename= loc.filename
        ; line= loc.line_number
        ; characters= Some (loc.start_char, loc.end_char)
        ; line_end= None
        ; name= Slot.name slot
        ; is_inline= Some (Slot.is_inline slot)
        ; is_raise= Some (Slot.is_raise slot)
        }

let fold_dedup_frames revlst item =
  match revlst with last :: _ when last = item -> revlst | _ -> item :: revlst

let frames_of_slots interop slots =
  slots
  |> Array.to_seq
  |> Seq.filter_map (frame_of_slot my_name)
  |> Seq.fold_left fold_dedup_frames (List.rev interop)
  |> List.rev

let of_raw_and_interop bt interop =
  bt
  |> Printexc.backtrace_slots
  |> Option.fold ~none:[] ~some:(frames_of_slots interop)

let of_raw bt = of_raw_and_interop bt empty

let get_backtrace_411 () = Printexc.get_raw_backtrace () |> of_raw

let make process filename line =
  {
    process
  ; filename
  ; line
  ; characters= None
  ; line_end= None
  ; name= None
  ; is_inline= None
  ; is_raise= None
  }

module Interop = struct
  (* This matches xapi.py:exception *)
  type error = {
      error: string
    ; (* Python json.dumps and rpclib are not very friendly *)
      files: string list
    ; lines: int list
  }
  [@@deriving rpc]

  let of_json source_name txt =
    txt |> Jsonrpc.of_string |> error_of_rpc |> fun e ->
    List.combine e.files e.lines
    |> List.map (fun (filename, line) -> make source_name filename line)

  let extractors = Atomic.make []

  let rec register f =
    let old = Atomic.get extractors in
    let next = f :: old in
    if not (Atomic.compare_and_set extractors old next) then (
      Thread.yield () ; register f
    )
end

let of_raw_extract exn bt =
  let interop =
    Interop.extractors
    |> Atomic.get
    |> List.find_map (fun f -> f exn)
    |> Option.value ~default:empty
  in
  of_raw_and_interop bt interop
