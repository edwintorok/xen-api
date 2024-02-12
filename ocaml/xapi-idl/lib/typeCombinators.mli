(*
 * Copyright (C) Cloud Software Group
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

type dict = (string * string) list

val typ_of_dict : (string * string) list Rpc.Types.typ

val dict : (string * string) list Rpc.Types.def

val option :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def
  -> 'a option Rpc.Types.def

val list :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def
  -> 'a list Rpc.Types.def

val pair :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def * 'b Rpc.Types.def
  -> ('a * 'b) Rpc.Types.def

val triple :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def * 'b Rpc.Types.def * 'c Rpc.Types.def
  -> ('a * 'b * 'c) Rpc.Types.def

val abstract :
     ?name:string
  -> ?description:string list
  -> ?test_data:'b list
  -> ('a -> 'b)
  -> ('b -> 'a)
  -> 'a Rpc.Types.def
  -> ('a -> Sexplib.Sexp.t)
  -> (Sexplib.Sexp.t -> 'a)
  -> 'b Rpc.Types.def * ('b -> Sexplib.Sexp.t) * (Sexplib.Sexp.t -> 'b)
(** [abstract ?name ?description of_other to_other typ_of_other sexp_of_other other_of_sexp] are serialization and deserialization functions
  for an abstract type given conversion functions and serialization/deserialization functions for another type. *)

module type Convertable = sig
  type t

  val typ_of : t Rpc.Types.typ
  (** [typ_of] is the {!type:Rpc.Types.typ} description on how to convert {!type:t} to and from {!type:Rpc.t} *)

  val t : t Rpc.Types.def
  (** [t] is the {!type:Rpc.Types.def} description of a {!type:t} convertible to {!type:Rpc.t} *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t t] converts [t] to {!type:Sexplib.Sexp.t} *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts [sexp] to {!type t} *)
end

module type OrderedType = sig
  include Map.OrderedType

  include Convertable with type t := t
end

module MakeMap (Ord : OrderedType) : sig
  include Map.S with type key = Ord.t

  val t :
       ?name:string
    -> ?description:string list
    -> 'a Rpc.Types.def
    -> 'a t Rpc.Types.def
  (** [t ?name ?description element] is the {!type:Rpc.Types.def} for a map with {!type:Ord.t} keys and [element] values. *)

  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  (** [sexp_of_t sexp_of_element t] converts a [t] map to {!type:Sexplib.Sexp.t} using [sexp_of_element] to convert values. *)

  val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
  (** [t_of_sexp element_of_sexp sexp] converts a [sexp] to a {!type t} map using [element_of_sexp] to convert values. *)
end

module MakeSet (Ord : OrderedType) : sig
  include Set.S with type elt = Ord.t

  include Convertable with type t := t
end
