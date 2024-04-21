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
(* Test program to subscribe to an event stream *)

let host = ref "localhost"

let port = ref 8086

let username = ref "root"

let password = ref ""

(* The interface to the ocaml client bindings requires a function which performs the XMLRPC call: *)
let rpc xml =
  let open Xmlrpc_client in
  let http = xmlrpc ~version:"1.0" "/" in
  XMLRPC_protocol.rpc ~srcstr:"event_listen" ~dststr:"xapi"
    ~transport:(TCP (!host, !port))
    ~http xml

open Client
open Event_types

let _ =
  Arg.parse
    [
      ("-h", Arg.Set_string host, "hostname to connect to")
    ; ("-p", Arg.Set_int port, "port number to connect to")
    ; ("-u", Arg.Set_string username, "username to connect with")
    ; ("-pw", Arg.Set_string password, "password to connect with")
    ]
    (fun x -> Printf.printf "Skipping unknown argument: %s" x)
    "Subscribe to an event stream and print the results" ;
  Printf.printf "Connecting to Host: %s; Port: %d; Username: %s" !host !port
    !username ;
  (* Interesting event stuff starts here: *)
  let session_id =
    Client.Session.login_with_password ~rpc ~uname:!username ~pwd:!password
      ~version:"1.2" ~originator:"event_listen"
  in
  let classes = ["*"] in
  let token = ref "" in
  while true do
    let event_from =
      event_from_of_rpc
        (Client.Event.from ~rpc ~session_id ~timeout:30. ~classes ~token:!token)
    in
    token := event_from.token ;
    List.iter
      (fun event -> print_endline (string_of_event event))
      event_from.events ;
    flush stdout
  done
