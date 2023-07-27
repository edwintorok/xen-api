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

(* Pre-generate some API calls that can be used to test XAPI's behaviour under load.
   These pre-generated API call can then be run using multiple engines: wrk/wrk2 with the provided [time.lua] script,
   or (in the future) an Eio based engine, etc.
   wrk/wrk2 is used as a load generator because they are not influenced by GC latencies, or other inefficiencies in the existing OCaml HTTP stack:
   any latencies or performance issues should be due to XAPI. However wrk/wrk2 doesn't allow us to instrument everything we'd like to in the request lifecycle
   (e.g. it only instruments successful replies).
   The load generator will send appropriate HTTP [traceparent] headers, and collect timing information,
   that can be converted into distributed tracing format.
*)
module Ze = Zero_http.Zero_events

let write_calls n call uri_path conv =
  let filename = String.concat "." [call.Rpc.name; uri_path] in
  Out_channel.with_open_text filename @@ fun oc ->
  Printf.fprintf oc "POST /%s\n" uri_path ;
  for _ = 1 to n do
    (* we need to repeat the conversion call, because JSONRPC has a builtin counter *)
    let str = conv call in
    assert (not @@ String.contains str '\n') ;
    Out_channel.output_string oc str ;
    Out_channel.output_char oc '\n'
  done

let n = 1000

let t = Speculative.init ()
let host = "perfuk-18-06d.xenrt.citrite.net"
let addr = (Unix.getaddrinfo host "80" [] |> List.hd).Unix.ai_addr
let conn = Speculative.connect t addr

(*module I =
  struct
    type !'a t = Real of 'a | Blocked
    let run = function | Real x -> x | Blocked -> failwith "TODO"
  end

module F = Freer.Make(I)
module M = struct
  include F
  let (>>|) x y = fmap y x
end*)

module M = struct
  type 'a t = Blocked

  let (>>|) _ _ = Blocked
end

let rpc call : Rpc.response M.t =
  (* write_calls n call "jsonrpc" Jsonrpc.string_of_call;
     write_calls n call "RPC2" Xmlrpc.string_of_call; *)
  let str = Jsonrpc.string_of_call call in
  let buf = Buffer.create 1024 in
  Printf.bprintf buf "POST /jsonrpc HTTP/1.1\r\nHost: %s\r\nContent-Length: %d\r\n\r\n" host (String.length str);
  Speculative.Connection.write conn (Buffer.contents buf);
  Speculative.Connection.write conn str;
  M.Blocked
  (*let buf = Bytes.create 8192 in
  let decode_response nread =
    Bytes.sub_string buf 0 nread |> Jsonrpc.response_of_string
  in
   let open M in
  let read_response = decode_response <$> Speculative.read conn buf in
  Speculative.write conn str >>> read_response *)

module C = Client.ClientF (M)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  let pwd = In_channel.with_open_text "/tmp/.pwd" In_channel.input_all |> String.trim in
 (* TODO: flag Logs.set_level ~all:true (Some Logs.Debug); *)
  (* we need to loop here, because jsonrpc would carry an ID that needs to be changed with each request *)
  let version = Xapi_version.version in
  Ze.(write Zero_http.url_full @@ "http://" ^ host ^ "/") ;
  Ze.(write Zero_http.url_method `POST);
  for i = 1 to 100 do
    let (_: _ M.t) =
      C.Session.login_with_password ~rpc ~uname:"root" ~pwd ~version
        ~originator:__FILE__
    in
    ()
  done;
  Speculative.run t
