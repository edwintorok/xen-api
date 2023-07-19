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

module Timestamp = Runtime_events.Timestamp

module MakeType (T : sig
  (** a small marshalable type. In marshaled form it must fit into 1024 bytes. *)
  type t
end) =
struct
  type t = T.t
  let marshal_flags = [Marshal.No_sharing]

  let encode bytes (span : T.t) =
    Marshal.to_buffer bytes 0 (Bytes.length bytes) span marshal_flags

  let decode bytes len : T.t =
    assert (len >= Marshal.header_size) ;
    assert (len >= Marshal.total_size bytes 0) ;
    Marshal.from_bytes bytes 0

  let typ = Runtime_events.Type.register ~encode ~decode
end

let spans = Hashtbl.create 1000

type attr = String of string | Bool of bool | Double of float | Int64 of int64

module SpanId = struct
  type t = int

  let ids = Atomic.make 0

  let invalid = -1

  let root = 0

  let next () = Atomic.fetch_and_add ids 1
end

type current_span = {
    attributes: (string * attr) Stack.t
  ; events: (string * Runtime_events.Timestamp.t) Stack.t
  ; mutable start_time_unix_nano: int64
  ; id: SpanId.t
  ; parent: SpanId.t
}

let make_span ~parent =
  {
    attributes= Stack.create ()
  ; events= Stack.create ()
  ; id= SpanId.invalid
  ; start_time_unix_nano= -1L
  ; parent
  }

let handlers = Stack.create ()

let spans = Hashtbl.create 127

let domains = Array.make 128 None

let get_current_span domain = domains.(domain)

let register typ process_event =
  let register_user_event callbacks =
    Runtime_events.Callbacks.add_user_event typ process_event callbacks
  in
  Stack.push register_user_event handlers

module MakeAttribute (T : sig
  type t

  val name : string

  val typ : t Runtime_events.Type.t

  val to_attribute : t -> attr
end) =
struct
  type Runtime_events.User.tag += Attribute

  let user = Runtime_events.User.register T.name Attribute T.typ

  let process_attribute span timestamp user t =
    let name = Runtime_events.User.name user in
    Stack.push (name, T.to_attribute t) span.attributes ;
    Stack.push (name, timestamp) span.events

  let process_attribute_event domain timestamp user value =
    match Runtime_events.User.tag user with
    | Attribute ->
        let span = get_current_span domain |> Option.get in
        process_attribute span timestamp user value
    | _ ->
        ()

  let () = register T.typ process_attribute_event
end

let emit_span = ref ignore

module MakeSpan (T : sig
  val name : string
end) =
struct
  type Runtime_events.User.tag += CustomSpan

  let user =
    Runtime_events.User.register T.name CustomSpan Runtime_events.Type.span

  let process_span_event domain timestamp user value =
    match Runtime_events.User.tag user with
    | CustomSpan -> (
        let span = get_current_span domain |> Option.get in
        match value with
        | Runtime_events.Type.Begin ->
            (* TODO: sync+adjust to realtime *)
            span.start_time_unix_nano <-
              Runtime_events.Timestamp.to_int64 timestamp
        | End ->
            assert (span.start_time_unix_nano <> -1L) ;
            !emit_span span ;
            Hashtbl.remove spans span.id ;
            domains.(domain) <- None
      )
    | _ ->
        ()

  let () = register Runtime_events.Type.span process_span_event
end

let register_user_events callbacks = Stack.fold ( |> ) callbacks handlers

module CurrentSpan = struct
  type Runtime_events.User.tag += NewSpan | SetCurrent

  let set_current =
    Runtime_events.User.register "zero_events.set_current" SetCurrent
      Runtime_events.Type.int

  let new_span =
    Runtime_events.User.register "zero_events.new_span" NewSpan
      Runtime_events.Type.int

  let process_int domain _timestamp user value =
    match Runtime_events.User.tag user with
    | NewSpan ->
        let parent =
          match get_current_span domain with
          | None ->
              SpanId.root
          | Some span ->
              span.id
        in
        let span = make_span ~parent in
        Hashtbl.add spans value span ;
        domains.(domain) <- Some span
    | SetCurrent ->
        domains.(domain) <- Some (Hashtbl.find spans value)
    | _ ->
        ()

  let () = register Runtime_events.Type.int process_int
end

module SmallString = MakeType(String)

let string_attribute name =
  let module M = MakeAttribute(struct
  type t = string
  let name = name
  let typ = SmallString.typ
  let to_attribute str = String str
  end) in
  M.user

let int_attribute name =
  let module M = MakeAttribute(struct
  type t = int
  let name = name
  let typ = Runtime_events.Type.int
  let to_attribute int = Int64 (Int64.of_int int)
  end) in
  M.user

let make_span name =
  let module M = MakeSpan(struct let name = name end) in
  M.user

let traceparent = string_attribute "traceparent"

let connect = make_span "zero_events.connect"

let connecting = User.register "zero_events.connecting" Connecting sockaddr

type User.tag += Connected

let connected = User.register "zero_events.connected" Connected Type.int

type User.tag += HttpRequestUrl

let http_request_url =
  User.register "url.full" HttpRequestUrl small_string (* url *)

type User.tag += HttpRequestMethod

let http_request_method =
  User.register "http.request.method" HttpRequestMethod type_http_request_method

type User.tag += HttpRequestBodySize

let http_request_body_size =
  User.register "http.request.body.size" HttpRequestBodySize Type.int

type User.tag += HttpResponseHeaders

let http_response_headers =
  User.register "zero_events.http.response.headers" HttpResponseHeaders
    Type.span

type User.tag += HttpStatusCode

let http_response_status_code =
  User.register "http.response.status_code" HttpStatusCode Type.int

type User.tag += ContentLength

let http_response_body_size =
  User.register "http.response.body_size" ContentLength Type.int

type span = Type.span = Begin | End

type User.tag += HttpResponseBody

let http_response_body =
  User.register "zero_events.http.response.body" HttpResponseBody Type.span

type User.tag += RpcSystem

let rpc_system = User.register "rpc.system" RpcSystem type_rpc_system

(* this allocates but we call it once on startup *)
let now_ns () =
  let d, ps = Ptime_clock.now_d_ps () in
  let d_in_ns = Int64.(mul 86_400_000_000_000L @@ of_int d) in
  let r = Int64.add d_in_ns (Int64.div ps 1000L) in
  Printf.printf "now: %Ld\n" r ;
  r

(* need to sync the monotonic timestamp with a well defined clock, so that we can correlate timestamps across hosts *)
type User.tag += Realtime

let realtime = User.register "realtime" Realtime int64

type User.tag += Nop

let nop = User.register "nop" Nop Type.unit

let () =
  (* sample the monotonic and absolute clock multiple times, so that we can measure using monotonic clock,
     but report using the absolute clock *)
  Runtime_events.User.write nop () ;
  Runtime_events.User.write realtime (now_ns ()) ;
  Runtime_events.User.write realtime (now_ns ()) ;
  Runtime_events.User.write nop () ;
  Runtime_events.User.write nop ()

type 'a callback = int -> Timestamp.t -> 'a User.t -> 'a -> unit

let register_callbacks callbacks ~traceparent ~http_request_url
    ~http_request_body_size ~request_id ~connected ~http_response_status_code
    ~http_response_body_size ~connecting ~http_request_method
    ~http_response_headers ~http_response_body ~rpc_system ~nop ~realtime =
  let add typ f callbacks =
    Runtime_events.Callbacks.add_user_event typ f callbacks
  in
  let handle_small_string domain timestamp user value =
    match User.tag user with
    | Traceparent ->
        traceparent domain timestamp user value
    | HttpRequestUrl ->
        http_request_url domain timestamp user value
    | _ ->
        ()
  in
  let handle_int domain timestamp user value =
    match User.tag user with
    | Request_id ->
        request_id domain timestamp user value
    | Connected ->
        connected domain timestamp user value
    | HttpRequestBodySize ->
        http_request_body_size domain timestamp user value
    | HttpStatusCode ->
        http_response_status_code domain timestamp user value
    | ContentLength ->
        http_response_body_size domain timestamp user value
    | _ ->
        ()
  in
  let handle_rpc_system domain timestamp user value =
    rpc_system domain timestamp user value
  in
  let handle_sockaddr domain timestamp user value =
    connecting domain timestamp user value
  in
  let handle_http_request_method domain timestamp user value =
    http_request_method domain timestamp user value
  in
  let handle_span domain timestamp user value =
    match User.tag user with
    | HttpResponseHeaders ->
        http_response_headers domain timestamp user value
    | HttpResponseBody ->
        http_response_body domain timestamp user value
    | _ ->
        ()
  in
  let handle_int64 domain timestamp user value =
    realtime domain timestamp user value
  in
  let handle_unit domain timestamp user value =
    nop domain timestamp user value
  in
  callbacks
  |> add small_string handle_small_string
  |> add Type.int handle_int
  |> add type_rpc_system handle_rpc_system
  |> add sockaddr handle_sockaddr
  |> add type_http_request_method handle_http_request_method
  |> add Type.span handle_span
  |> add int64 handle_int64
  |> add Type.unit handle_unit
