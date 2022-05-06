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

(** A Context is used to represent every API invocation. It may be extended
    to include extra data without changing all the autogenerated signatures *)
type t

type origin = Http of Http.Request.t * Unixfd.t | Internal

(** {6 Constructors} *)

val make :
     ?http_other_config:(string * string) list
  -> ?quiet:bool
  -> ?subtask_of:API.ref_task
  -> ?session_id:API.ref_session
  -> ?database:Db_ref.t
  -> ?task_in_database:bool
  -> ?task_description:string
  -> ?origin:origin
  -> string
  -> t
(** [make ~__context ~subtask_of ~database ~session_id ~task_in_database ~task_description ~origin task_name] creates a new context.
    [http_other_config] are extra bits of context picked up from HTTP headers,
    [quiet] silences "task created" log messages,
    [subtask_of] is a reference to the parent task,
    [session_id] is the current session id,
    [database] is the database to use in future Db.* operations
    [origin] indicates whether the operation is triggered by an HTTP request or by an internal operation
    [task_in_database] indicates if the task needs to be stored the task in the database,
    [task_description] is the description of the task (Task.name_description),
    [task_name] is the task name of the created context (Task.name_label). *)

val of_http_req :
     ?session_id:API.ref_session
  -> ?internal_async_subtask:bool
  -> generate_task_for:bool
  -> supports_async:bool
  -> label:string
  -> http_req:Http.Request.t
  -> fd:Unixfd.t
  -> unit
  -> t

val from_forwarded_task :
     ?http_other_config:(string * string) list
  -> ?session_id:API.ref_session
  -> ?origin:origin
  -> API.ref_task
  -> t

val make_subcontext : __context:t -> ?task_in_database:bool -> string -> t

(** {6 Accessors} *)

val get_session_id : t -> API.ref_session
(** [get_session_id __context] returns the session id stored in [__context]. In case there is no session id in this
    context, it fails with [Failure "Could not find a session_id"]. *)

val get_task_id : t -> API.ref_task
(** [get_task_id __context] returns the task id stored in [__context]. Such a task can be either a task stored in
    database or a tempory task (also called dummy). *)

val forwarded_task : t -> bool

val string_of_task : t -> string

val task_in_database : t -> bool
(** [task_in_database __context] indicates if [get_task_id __context] corresponds to a task stored in database or
    to a dummy task. *)

val get_origin : t -> string
(** [get_origin __context] returns a string containing the origin of [__context]. *)

val database_of : t -> Db_ref.t
(** [database_of __context] returns a database handle, which can be used by Db.* *)

(** {6 Destructors} *)

val destroy : t -> unit

(** {6 Auxiliary functions } *)

val _client_of_rq : Http.Request.t -> Ipaddr.t option

val is_unix_socket : Unix.file_descr -> bool
(** [is_unix_socket fd] *)

val preauth : __context:t -> [`root | `client_cert] option
(** [preauth ~__context] *)

val trackid_of_session :
  ?with_brackets:bool -> ?prefix:string -> API.ref_session option -> string

val trackid : ?with_brackets:bool -> ?prefix:string -> t -> string

val check_for_foreign_database : __context:t -> t

val get_http_other_config : Http.Request.t -> (string * string) list

(** {6 Functions which help resolving cyclic dependencies} *)

val __get_task_name : (__context:t -> API.ref_task -> string) ref

val __destroy_task : (__context:t -> API.ref_task -> unit) ref

val __make_task :
  (   __context:t
   -> http_other_config:(string * string) list
   -> ?description:string
   -> ?session_id:API.ref_session
   -> ?subtask_of:API.ref_task
   -> string
   -> API.ref_task * API.ref_task Uuid.t
  )
  ref

val set_test_rpc : t -> (Rpc.call -> Rpc.response) -> unit

val get_test_rpc : t -> (Rpc.call -> Rpc.response) option

val set_test_clusterd_rpc : t -> (Rpc.call -> Rpc.response) -> unit

val get_test_clusterd_rpc : t -> (Rpc.call -> Rpc.response) option

val get_client : t -> string option

val get_client_ip : t -> string option

val get_user_agent : t -> string option
