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

open Safe_resources

type handler = {
    name: string  (** used for naming the thread *)
  ; body: Unix.sockaddr -> Unixfd.t -> unit
        (** function called in a thread for each connection*)
}

type server = {
    shutdown: unit -> unit  (** clean shutdown, blocks until thread has gone *)
}

val server : handler -> Unix.file_descr -> server
(** Creates a server given a bound socket and a handler *)

val set_max_accepted_connections : int -> unit
(** [set_max_accepted_connections n] sets a limit for the maximum number of accepted connections.
      Can only be called once.
      When the limit is hit new connections won't be [accept]ed on the socket until an existing
      connection is closed, thus limiting the number of open file descriptors in the process.

      It is recommended to set this limit to at most half of RLIMIT_NOFILE, so that the code
      handling the connection can still open at least one more file descriptor should it need to
      (many library functions may internally use file descriptors).

      The limit only applies to connections {!accept}ed after this function is called.

      The semaphore is acquired *before* accepting the connection,
      i.e. before we open a new file descriptor.
      File descriptors are a global and limited resource, therefore this is a single global semaphore.
      More fine grained limits can be implemented by XAPI once the connection is accepted,
      e.g. load shedding only on TCP connections, but not Unix domain connections.

      This will wait until one of the already in progress requests finishes, and builds up a
      backlog on the underlying socket.
      The OS may then start rejecting connections at the TCP level if that happens.
  *)
