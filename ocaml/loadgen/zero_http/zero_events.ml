(*
 * Copyright (C) 2023 Cloud Software Group
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

(* TODO: select based on compiler version, for now just 5.1+ version *)

(* Zero allocation when recording events.
   Allocation is unavoidable when decoding, but that can happen in a separate process. *)

(* just very basic: marshal/unmarshal, and let the post-processor handle building attributes/etc. *)

let int64 =
  let encode bytes value =
    Bytes.set_int64_ne bytes 0 value ;
    8
  in
  let decode bytes len =
    assert (len = 8) ;
    Bytes.get_int64_ne bytes 0
  in
  Runtime_events.Type.register ~encode ~decode

let handlers = Stack.create ()

let register typ process_event =
  let register_user_event callbacks =
    Runtime_events.Callbacks.add_user_event typ process_event callbacks
  in
  Stack.push register_user_event handlers

module Timestamp = struct
  type Runtime_events.User.tag += Clock

  (* delta between monotonic clock and absolute unix epoch in nanoseconds *)
  let delta = ref (-1L)

  let () =
    let clock = Runtime_events.User.register "zero_events.clock" Clock int64 in
    (* this allocates but we call it once on startup *)
    let now_ns =
      let d, ps = Ptime_clock.now_d_ps () in
      let d_in_ns = Int64.(mul 86_400_000_000_000L @@ of_int d) in
      Int64.add d_in_ns (Int64.div ps 1000L)
    in
    Runtime_events.User.write clock now_ns ;
    register int64 @@ fun _domain timestamp user value ->
    match Runtime_events.User.tag user with
    | Clock ->
        delta := Int64.sub value @@ Runtime_events.Timestamp.to_int64 timestamp
    | _ ->
        ()

  let to_unix_nano t =
    let offset = !delta in
    assert (offset > 0L) ;
    Int64.add offset @@ Runtime_events.Timestamp.to_int64 t
end

type 'a callback = domain:int -> timestamp_unix_ns:int64 -> name:string -> value:'a -> unit
type 'a event = 'a Runtime_events.User.t
let marshal_flags = [Marshal.No_sharing]

let register_marshaled_event (type t) name
    ~(on_process_event : t callback) =
  let encode bytes data =
    Marshal.to_buffer bytes 0 (Bytes.length bytes) data marshal_flags
  in
  let decode bytes len : t =
    assert (len >= Marshal.header_size) ;
    assert (len >= Marshal.total_size bytes 0) ;
    Marshal.from_bytes bytes 0
  in
  let typ = Runtime_events.Type.register ~encode ~decode in
  let module M = struct
    type Runtime_events.User.tag += Marshaled

    let user = Runtime_events.User.register name Marshaled typ

    let process_event domain timestamp user value =
      match Runtime_events.User.tag user with
      | Marshaled ->
          on_process_event ~domain
            ~timestamp_unix_ns:(Timestamp.to_unix_nano timestamp)
            ~name:(Runtime_events.User.name user)
            ~value
      | _ ->
          ()

    let () = register typ process_event
  end in
  M.user

type span = Runtime_events.Type.span = Begin | End
type Runtime_events.User.tag += SimpleSpan

let register_simple_span name =
  Runtime_events.User.register name SimpleSpan Runtime_events.Type.span  

let emit ev value = Runtime_events.User.write ev value

let register_callbacks ~on_simple_span callbacks =
  let on_simple_span domain timestamp user value =
    match Runtime_events.User.tag user with
    | SimpleSpan ->
      on_simple_span ~domain ~timestamp_unix_ns:(Timestamp.to_unix_nano timestamp) ~name:(Runtime_events.User.name user) ~value
    | _ -> ()
  in
  Stack.fold (|>) callbacks handlers
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.span on_simple_span