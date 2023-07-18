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

module User = Runtime_events.User
module Type = Runtime_events.Type

let sockaddr =
 let marshal_flags = [Marshal.No_sharing] in
 let encode bytes (addr:Unix.sockaddr) =
  (* inet_addr is abstract, but internally a string, so it is marshalable *)
  Marshal.to_buffer bytes 0 (Bytes.length bytes) addr marshal_flags
 in
 let decode bytes len : Unix.sockaddr =
  Marshal.from_bytes bytes len
 in
 Type.register ~encode ~decode

 let encode_small_string bytes str =
  let n = String.length str in
  Bytes.blit_string str 0 bytes 0 n;
  n

 let decode_small_string bytes len =
  Bytes.sub_string bytes 0 len

let small_string =
 Type.register ~encode:encode_small_string ~decode:decode_small_string

let http_request_method =
 let encode bytes meth =
  encode_small_string bytes (Http.Method.to_string meth)
 in
 let decode bytes len =
  decode_small_string bytes len |> Http.Method.of_string
 in
 Type.register ~encode ~decode

type rpc_system =
 | XmlRPC
 | JsonRPC2

let rpc_system =
 let encode bytes r =
  let i = match r with
  | XmlRPC -> 0
  | JsonRPC2 -> 1
  in
  Bytes.set_uint8 bytes 0 i;
  1
 in
 let decode bytes len =
  assert (len == 1);
  match Bytes.get_uint8 bytes 0 with
  | 0 -> XmlRPC
  | 1 -> JsonRPC2
  | n -> invalid_arg (Printf.sprintf "Invalid rpc system decoded: %d" n)
 in
 Type.register ~encode ~decode

let int64 =
  let encode bytes i =
   Bytes.set_int64_ne bytes 0 i;
   8
  in
  let decode bytes len =
   assert (len == 8);
   Bytes.get_int64_ne bytes 0
  in
  Type.register ~encode ~decode
 
type User.tag += Traceparent
let traceparent = User.register "traceparent" Traceparent small_string

type User.tag += Request_id
let request_id = User.register "zero_events.request_id" Request_id Type.int

type User.tag += Connecting
let connecting = User.register "zero_events.connecting" Connecting sockaddr

type User.tag += Connected
let connected = User.register "zero_events.connected" Connected Type.int

type User.tag += HttpRequestUrl
let http_request_url = User.register "url.full" HttpRequestUrl small_string (* url *)

type User.tag += HttpRequestMethod
let http_request_method = User.register "http.request.method" HttpRequestMethod http_request_method

type User.tag += HttpRequestBodySize
let http_request_body_size = User.register "http.request.body.size" HttpRequestBodySize Type.int

type User.tag += HttpResponseHeaders
let http_response_headers = User.register "zero_events.http.response.headers" HttpResponseHeaders Type.span

type User.tag += HttpStatusCode
let http_response_status_code = User.register "http.response.status_code" HttpStatusCode Type.int

type User.tag += ContentLength
let http_response_body_size = User.register "http.response.body_size" ContentLength Type.int

type span = Type.span = Begin | End
type User.tag += HttpResponseBody
let http_response_body = User.register "zero_events.http.response.body" HttpResponseBody Type.span

type User.tag += RpcSystem
let rpc_system = User.register "rpc.system" RpcSystem rpc_system

(* this allocates but we call it once on startup *)
let now_ns () =
 let d, ps = Ptime_clock.now_d_ps () in
 let d_in_ns = Int64.(mul 86400L @@ of_int d) in
 Int64.add d_in_ns (Int64.div ps 1000L) 

(* need to sync the monotonic timestamp with a well defined clock, so that we can correlate timestamps across hosts *)
type User.tag += Realtime
let realtime = User.register "realtime" Realtime int64

type User.tag += Nop
let nop = User.register "nop" Nop Type.unit

let () =
 (* sample the monotonic and absolute clock multiple times, so that we can measure using monotonic clock,
    but report using the absolute clock *)
 Runtime_events.User.write nop ();
 Runtime_events.User.write realtime (now_ns ());
 Runtime_events.User.write realtime (now_ns ());
 Runtime_events.User.write nop ();
 Runtime_events.User.write nop ()