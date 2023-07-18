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
module Timestamp = Runtime_events.Timestamp

let sockaddr =
 let marshal_flags = [Marshal.No_sharing] in
 let encode bytes (addr:Unix.sockaddr) =
  (* inet_addr is abstract, but internally a string, so it is marshalable *)
  let n=  Marshal.to_buffer bytes 0 (Bytes.length bytes) addr marshal_flags in
  n
 in
 let decode bytes len : Unix.sockaddr =
  Marshal.from_bytes bytes 0
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

let type_http_request_method =
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

let type_rpc_system =
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
let http_request_method = User.register "http.request.method" HttpRequestMethod type_http_request_method

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
let rpc_system = User.register "rpc.system" RpcSystem type_rpc_system

(* this allocates but we call it once on startup *)
let now_ns () =
 let d, ps = Ptime_clock.now_d_ps () in
 let d_in_ns = Int64.(mul 86_400_000_000_000L @@ of_int d) in
 let r = Int64.add d_in_ns (Int64.div ps 1000L)  in
 Printf.printf "now: %Ld\n" r;
 r

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
 
type 'a callback = int -> Timestamp.t -> 'a User.t -> 'a -> unit
let register_callbacks callbacks ~traceparent ~http_request_url ~http_request_body_size ~request_id ~connected ~http_response_status_code ~http_response_body_size ~connecting ~http_request_method ~http_response_headers ~http_response_body ~rpc_system ~nop ~realtime =
 let add typ f callbacks = Runtime_events.Callbacks.add_user_event typ f callbacks in
 let handle_small_string domain timestamp user value =
  match User.tag user with
  | Traceparent -> traceparent domain timestamp user value
  | HttpRequestUrl -> http_request_url domain timestamp user value
  | _ -> ()
 in
 let handle_int domain timestamp user value =
  match User.tag user with
  | Request_id -> request_id domain timestamp user value
  | Connected -> connected domain timestamp user value
  | HttpRequestBodySize -> http_request_body_size domain timestamp user value
  | HttpStatusCode -> http_response_status_code domain timestamp user value
  | ContentLength -> http_response_body_size domain timestamp user value
  | _ -> ()
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
  | HttpResponseHeaders -> http_response_headers domain timestamp user value
  | HttpResponseBody -> http_response_body domain timestamp user value 
  | _ -> ()
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
