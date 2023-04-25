(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

val make : ?timeout:float -> string -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

val make_json : ?timeout:float -> string -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

include module type of Client.ClientF (Lwt)

module Lwt_unix_IO : sig
  type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel

  type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel

  val open_connection : Uri.t -> (ic * oc, exn) Xen_api.result Lwt.t
end

module SessionCache : sig
  (** a session cache for a specific user *)
  type t

  type target = [`Local | `IP of string | `RPC of (Rpc.call -> Rpc.response Lwt.t)]

  val create :
       ?timeout:float
    -> target:[< target]
    -> uname:string
    -> pwd:string
    -> version:string
    -> originator:string
    -> unit
    -> t
  (** [create ?timeout ~target_ip ~uname ~pwd ~version ~originator ()] is a session cache for the specified [uname] user.
      
    A session is retrieved and an RPC function is constructed for the given [target] host
    using the latest protocol (currently JSON).
  
    @param timeout timeout for API calls in seconds
    @param target XAPI endpoint, [`Local] to use the UNIX domain socket, or [`IP host] otherwise
    @param uname XAPI user name
    @param pwd password for [uname]
    @param version client application version
    @param originator client application user agent
  *)

  val with_session :
       t
    -> (   rpc:(Rpc.call -> Rpc.response Lwt.t)
        -> session_id:API.ref_session
        -> 'a Lwt.t
       )
    -> 'a Lwt.t
  (** [with_session t f] calls [f ~rpc ~session_id] with a valid logged in session and an [rpc] function to use. *)

  val destroy : t -> unit Lwt.t
  (** [destroy t] logs out all sessions from the cache *)
end
