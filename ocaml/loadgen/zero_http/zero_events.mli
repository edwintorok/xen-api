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

(** Zero-copy, low-overhead event recording.

 On OCaml 5.1+ this uses the built-in runtime-events.
 On OCaml <5.1 it uses its own ring-buffer and bechamel.monotonic_clock
   (mtime would allocate an int64 every time and not give us the absolute value, and gettimeofday would use a float)
*)
module User: sig
 type _ t
 type tag = ..

 val write : 'a t -> 'a -> unit
 (** [write t v] records a new event [t] with value [v]. *)

 val name: _ t -> string
 (** [name t] is a unique identifier for the name of the event [t] *)

 val tag: _ t -> tag
 (** [tag t] is the tag associated with event [t] when known. *)
end

type User.tag +=
 | Traceparent
 | Request_id
 | Connecting
 | Connected
 | HttpRequestUrl
 | HttpRequestMethod
 | HttpRequestBodySize
 | HttpResponseHeaders
 | HttpStatusCode
 | ContentLength
 | HttpResponseBody
 | RpcSystem
 | Realtime
 | Nop

type span = Begin | End

type rpc_system = XmlRPC | JsonRPC2

val traceparent: string User.t
val http_request_url : string User.t

val request_id: int User.t
val connected: int User.t
val http_response_status_code: int User.t
val http_response_body_size: int User.t
val http_request_body_size: int User.t

val connecting: Unix.sockaddr User.t

val http_request_method: Http.Method.t User.t

val http_response_headers: span User.t
val http_response_body: span User.t

val rpc_system : rpc_system User.t
val nop: unit User.t

val realtime: int64 User.t

module Timestamp: sig
 type t
 val to_int64: t -> int64
end

type 'a callback = int -> Timestamp.t -> 'a User.t -> 'a -> unit

val register_callbacks:
 Runtime_events.Callbacks.t ->
 traceparent:string callback
 -> http_request_url: string callback
 -> http_request_body_size: int callback
 -> request_id: int callback
 -> connected: int callback
 -> http_response_status_code: int callback
 -> http_response_body_size: int callback
 -> connecting: Unix.sockaddr callback
 -> http_request_method: Http.Method.t callback
 -> http_response_headers: span callback
 -> http_response_body: span callback
 -> rpc_system: rpc_system callback
 -> nop: unit callback
 -> realtime: int64 callback
 -> Runtime_events.Callbacks.t