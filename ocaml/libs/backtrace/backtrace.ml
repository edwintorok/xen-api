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

let my_name = ref (Filename.basename Sys.argv.(0))

let set_my_name x = my_name := x

module Mutex = struct
  include Mutex

  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    let r = try f () with exn -> Mutex.unlock lock ; raise exn in
    Mutex.unlock lock ; r
end

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i
    :: split_c c (String.sub str (i + 1) (String.length str - i - 1))
  with Not_found -> [str]

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
[@@deriving sexp]

type t = frame list [@@deriving sexp]

let empty = []

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

type table = {
    backtraces: t array
  ; exn_to_backtrace: exn Weak.t
  ; mutable producer: int (* free running counter *)
  ; m: Mutex.t
}

(* Increasing this makes 'find_all' slower and increases the amount of
   memory needed. Since we have a table per thread a small number should
   be enough. *)
let max_backtraces = 100

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
  |> Seq.filter_map (frame_of_slot !my_name)
  |> Seq.fold_left fold_dedup_frames (List.rev interop)
  |> List.rev

let of_raw_and_interop bt interop =
  bt
  |> Printexc.backtrace_slots
  |> Option.fold ~none:[] ~some:(frames_of_slots interop)

let of_raw bt = of_raw_and_interop bt empty

let get_backtrace_411 () = Printexc.get_raw_backtrace () |> of_raw

let make () =
  let backtraces = Array.make max_backtraces [] in
  let exn_to_backtrace = Weak.create max_backtraces in
  let producer = 0 in
  (* free running *)
  let m = Mutex.create () in
  {backtraces; exn_to_backtrace; producer; m}

let add t exn bt =
  Mutex.execute t.m (fun () ->
      let slot = t.producer mod max_backtraces in
      t.producer <- t.producer + 1 ;
      Weak.set t.exn_to_backtrace slot (Some exn) ;
      t.backtraces.(slot) <- bt
  )

let is_important t exn =
  let bt = get_backtrace_411 () in
  (* Deliberately clear the backtrace buffer *)
  (try raise Not_found with Not_found -> ()) ;
  add t exn bt

(* fold over the slots matching exn *)
let fold t exn f initial =
  let rec loop acc from =
    if from < 0 || t.producer - from > max_backtraces then
      acc
    else
      let slot = from mod max_backtraces in
      match Weak.get t.exn_to_backtrace slot with
      | Some exn' when exn' == exn ->
          loop (f acc slot) (from - 1)
      | _ ->
          loop acc (from - 1)
  in
  loop initial (t.producer - 1)

let remove_dups xs =
  List.fold_left
    (fun (last, acc) item -> (item, if last = item then acc else item :: acc))
    ([], []) (List.rev xs)
  |> snd

(*
  |> List.rev
*)
let get t exn =
  fold t exn (fun acc slot -> t.backtraces.(slot) :: acc) []
  |> remove_dups
  |> List.concat

let remove t exn =
  fold t exn
    (fun acc slot ->
      let bt = t.backtraces.(slot) in
      Weak.set t.exn_to_backtrace slot None ;
      t.backtraces.(slot) <- [] ;
      bt :: acc
    )
    []
  |> remove_dups
  |> List.concat

let per_thread_backtraces = Hashtbl.create 37

let per_thread_backtraces_m = Mutex.create ()

let with_lock f x =
  Mutex.lock per_thread_backtraces_m ;
  try
    let result = f x in
    Mutex.unlock per_thread_backtraces_m ;
    result
  with e ->
    Mutex.unlock per_thread_backtraces_m ;
    raise e

let with_backtraces f =
  let id = Thread.(id (self ())) in
  let tbl =
    with_lock
      (fun () ->
        let tbl =
          if Hashtbl.mem per_thread_backtraces id then
            Hashtbl.find per_thread_backtraces id
          else
            make ()
        in
        (* If we nest these functions we add multiple bindings
           to the same mutable table which is ok *)
        Hashtbl.add per_thread_backtraces id tbl ;
        tbl
      )
      ()
  in
  try
    let result = f () in
    with_lock (Hashtbl.remove per_thread_backtraces) id ;
    `Ok result
  with e ->
    let bt = get tbl e in
    with_lock (Hashtbl.remove per_thread_backtraces) id ;
    `Error (e, bt)

let with_table f default =
  let id = Thread.(id (self ())) in
  match
    with_lock
      (fun () ->
        if Hashtbl.mem per_thread_backtraces id then
          Some (Hashtbl.find per_thread_backtraces id)
        else
          None
      )
      ()
  with
  | None ->
      default ()
  | Some tbl ->
      f tbl

let is_important exn =
  with_table (fun tbl -> is_important tbl exn) (fun () -> ())

let add exn bt = with_table (fun tbl -> add tbl exn bt) (fun () -> ())

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

let warning () =
  [
    make !my_name
      (Printf.sprintf
         "(Thread %d has no backtrace table. Was with_backtraces called?"
         Thread.(id (self ()))
      )
      0
  ]

let remove exn = with_table (fun tbl -> remove tbl exn) warning

let get exn = with_table (fun tbl -> get tbl exn) warning

let reraise old newexn =
  add newexn (remove old) ;
  raise newexn

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
end
