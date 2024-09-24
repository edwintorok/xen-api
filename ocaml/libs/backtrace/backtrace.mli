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

(** A backtrace from a particular thread. *)
type t [@@deriving sexp]

val empty : t
(** An empty backtrace *)

val of_raw : Printexc.raw_backtrace -> t
(** [of_raw bt] convert a {!type:Printexc.raw_backtrace} to {!type:t}.
    This can be useful if you want to serialize it / deserialize it. *)

val of_raw_extract : exn -> Printexc.raw_backtrace -> t
(** [of_raw_extract exn raw] concatenates the [interop] backtrace with the [raw] OCaml one.
    The [interop] backtrace can be a parameter to an exception constructor, which can be extracted by an {!val:InterOp.register}ed extractor.
    This can be used before a call to [log_backtrace].
 *)

val to_string_hum : t -> string
(** Produce a human-readable printable/loggable version of the
    backtrace. *)

(** {2 Handling exceptions without losing backtraces}
    Whenever a function raises an exception, the backtrace buffer is
    emptied. Therefore when we are handling an exception, we must
    stash away a copy of the backtrace buffer if there is any risk
    of us raising another (or even the same) exception) *)

val try_with : (unit -> 'a) -> (exn -> Printexc.raw_backtrace -> 'a) -> 'a
(** [try_with f arg handler] runs [f arg], passing any exceptions together with its backtrace to [handler].
    This ensures that backtraces do not get overwritten before having a chance to retrieve them.

    It is equivalent to:
    {[
        try f ()
        with e -> handler e (Printexc.get_raw_backtrace ())
    ]}

    And prevents common mistakes like this where the exception would be overwritten:
    {[
        try f ()
        with e ->
         error "Foo: %..." ...;
         Backtrace.reraise e ...
    ]}
 *)

(** {2 Interop with other languages}
    This allows backtraces from other languages (e.g. python) to be converted
    into OCaml-style backtraces. *)

module Interop : sig
  val of_json : string -> string -> t
  (** [of_json source_name json]: unmarshals a json-format backtrace from
      [source_name] *)

  val register : (exn -> t option) -> unit
  (** [register extractor] registers a function that can extract the [interop] exception from an exception constructor.
      It should return [None] if it doesn't recognize the exception.
    *)
end
