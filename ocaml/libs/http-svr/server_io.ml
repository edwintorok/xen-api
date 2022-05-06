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

(* open Unix *)
open Xapi_stdext_pervasives.Pervasiveext
open Safe_resources

module D = Debug.Make (struct let name = "server_io" end)

open D

type handler = {
    name: string
  ; (* body should close the provided fd *)
    body: Unix.sockaddr -> Unixfd.t -> unit
}

let handler_by_thread (h : handler) (s : Unixfd.t)
    (caller : Unix.sockaddr) =
  Thread.create
    (fun () -> Debug.with_thread_named h.name (fun () -> h.body caller s) ())
    ()

(** Function with the main accept loop *)

exception PleaseClose

let set_intersect a b = List.filter (fun x -> List.mem x b) a

let semaphore = ref None

let set_max_accepted_connections n =
  match !semaphore with
| Some _ ->
    (* shouldn't happen: meant to be called once during startup after Xapi_globs is initialized *)
    invalid_arg "Semaphore already initialized"
| None -> semaphore := Some (Semaphore.Counting.make n)

let with_connection sock =
  Safe_resources.Unixfd.with_accept ?semaphore:!semaphore ~loc:__LOC__ sock

let establish_server ?(signal_fds = []) forker sock =
  while true do
    try
      let r, _, _ = Unix.select ([sock] @ signal_fds) [] [] (-1.) in
      (* If any of the signal_fd is active then bail out *)
      if set_intersect r signal_fds <> [] then raise PleaseClose ;
      with_connection sock @@ fun s caller ->
      try
        (* close on exec already set in Unixfd.accept *)
        ignore (forker s caller)
      with exc ->
        (* NB provided 'forker' is configured to make a background thread then the
           	     only way we can get here is if set_close_on_exec or Thread.create fails.
           	     This means we haven't executed any code which could close the fd therefore
           	     we should do it ourselves. *)
        debug "Got exception in server_io.ml: %s" (Printexc.to_string exc) ;
        log_backtrace () ;
        Unixfd.safe_close s;
        Thread.delay 30.0
    with
    | PleaseClose ->
        debug "Caught PleaseClose: shutting down server thread" ;
        raise PleaseClose
    | Unix.Unix_error (err, a, b) ->
        debug "Caught Unix exception in accept: %s in %s %s"
          (Unix.error_message err) a b ;
        Thread.delay 10.
    | e ->
        debug "Caught exception in except: %s" (Printexc.to_string e) ;
        Thread.delay 10.
  done

type server = {shutdown: unit -> unit}

let server handler sock =
  let status_out, status_in = Unix.pipe () in
  let toclose = ref [sock; status_in; status_out] in
  let close' fd =
    if List.mem fd !toclose then (
      toclose := List.filter (fun x -> x <> fd) !toclose ;
      try Unix.close fd (* not an [accept] socket *)
      with exn ->
        warn "Caught exn in Server_io.server: %s" (Printexc.to_string exn)
    ) else
      warn "Attempt to double-shutdown Server_io.server detected; ignoring"
  in
  let thread =
    Thread.create
      (fun () ->
        Debug.with_thread_named handler.name
          (fun () ->
            try
              establish_server ~signal_fds:[status_out]
                (handler_by_thread handler)
                sock
            with PleaseClose -> debug "Server thread exiting"
          )
          ()
      )
      ()
  in
  let shutdown () =
    finally
      (fun () ->
        let len = Unix.write status_in (Bytes.of_string "!") 0 1 in
        if len <> 1 then failwith "Failed to signal to server to shutdown" ;
        Thread.join thread
      )
      (fun () -> List.iter close' !toclose)
  in
  {shutdown}
