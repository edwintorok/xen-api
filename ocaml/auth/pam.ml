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

type pam_handle

external authenticate_start: unit -> pam_handle = "stub_XA_mh_authorize_start"
external authenticate_stop: pam_handle -> unit = "stub_XA_mh_authorize_stop"

external authorize : pam_handle -> string -> string -> unit = "stub_XA_mh_authorize"

(* TODO: make this configurable in Xapi_globs *)
 (* because this is initialized on startup this is not settable from a config file yet! *)
let auth_workers = Threadpool.create ~name:"PAM auth" authenticate_start authenticate_stop 8
(*
let () = at_exit (fun () -> Threadpool.shutdown auth_workers)
*)

let authenticate user password =
  Threadpool.run_in_pool auth_workers @@ fun handle ->
  authorize handle user password

external change_password : pam_handle -> string -> string -> unit = "stub_XA_mh_chpasswd"

let change_password user password =
 Threadpool.run_in_pool auth_workers @@ fun handle ->
 change_password handle user password
