(*
 * Copyright (C) Cloud Software Group, Inc.
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

open Xapi_metastore

(* TODO: allocate port dynamically *)
let peer_port = 12380

let port = 12379

let live =
  Config.Live.
    {
      advertise_client_urls=
        [Uri.make ~host:"localhost" ~port ~scheme:"http" ()]
        (* empty value not valid here *)
    ; initial_advertise_peer_urls=
        [Uri.make ~host:"localhost" ~port:peer_port ~scheme:"http" ()]
    }
  

let local =
  Config.Local.
    {
      config=
        {
          name= "localtest"
        ; enable_v2= false
        ; log_level= Some Config_field.DEBUG
        ; log_output= Some Stdout
        }
    ; peer= (None, [Uri.make ~host:"localhost" ~port:peer_port ()])
    ; client= (None, [Uri.make ~host:"localhost" ~port ()])
    }
  

let config = Config.make live local

let etcd = ref "/usr/bin/etcd"

let etcdctl = ref "/usr/bin/etcdctl"

module StringMap = Map.Make (String)

(* easier to debug when starting directly, without systemd, because we get the
   output directly *)
let test_start_stop_direct config () =
  Fe_systemctl.set_test () ;
  Logs.debug (fun m -> m "Starting with config %a" Config.dump config) ;
  let env = config |> Config.to_dict in
  let dump =
    Fmt.Dump.iter_bindings StringMap.iter Fmt.(any "map") Fmt.string Fmt.string
  in
  Logs.debug (fun m -> m "environment: %a" dump env) ;
  let uuid = Uuidx.make_uuid_urnd () |> Uuidx.to_string in
  let service = Printf.sprintf "etcd-test-%s" uuid in
  let properties = [("Type", ["notify"])] in
  Fe_systemctl.start_transient ~env ~properties ~service !etcd [] ;
  ( if not (Fe_systemctl.is_active ~service) then
      let (_ : int) =
        Sys.command
        @@ Printf.sprintf "systemctl status --user --lines=1000 %s" service
      in
      Alcotest.failf "Service %s is not running" service
  ) ;
  let _status = Fe_systemctl.stop ~service in
  if Fe_systemctl.is_active ~service then
    Alcotest.failf "Service %s is still running" service

let tests () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level ~all:true (Some Logs.Debug) ;
  [("start/stop etcd directly", `Quick, test_start_stop_direct config)]
