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

type dict = (string * string) list [@@deriving rpcty]

let option ?name ?(description = []) d =
  let open Rpc.Types in
  let name =
    Option.fold ~none:(Printf.sprintf "%s option" d.name) ~some:Fun.id name
  in
  {name; description; ty= Option d.ty}

let list ?name ?(description = []) d =
  let open Rpc.Types in
  let name =
    Option.fold ~none:(Printf.sprintf "list of %ss" d.name) ~some:Fun.id name
  in
  {name; description; ty= List d.ty}

let pair ?name ?(description = []) (p0, p2) =
  let open Rpc.Types in
  let name =
    Option.fold
      ~none:(Printf.sprintf "pair of %s and %s" p0.name p2.name)
      ~some:Fun.id name
  in
  {name; description; ty= Tuple (p0.ty, p2.ty)}

let triple ?name ?(description = []) (p1, p2, p3) =
  let open Rpc.Types in
  let name =
    Option.fold
      ~none:(Printf.sprintf "triple of %s, %s and %s" p1.name p2.name p3.name)
      ~some:Fun.id name
  in
  {name; description; ty= Tuple3 (p1.ty, p2.ty, p3.ty)}

let abstract_def ?name ?(description = []) ?test_data of_other to_other
    def_of_other =
  let open Rpc.Types in
  let name =
    Option.value name
      ~default:(Printf.sprintf "abstract type based on %s" def_of_other.name)
  in
  let typ = def_of_other.ty in
  let test_data =
    match test_data with
    | Some x ->
        x
    | None ->
        Rpc_genfake.gentest typ |> List.rev_map of_other
  in
  let typ_of =
    let rpc_of t = t |> to_other |> Rpcmarshal.marshal typ in
    let of_rpc rpc = rpc |> Rpcmarshal.unmarshal typ |> Result.map of_other in
    Rpc.Types.Abstract {aname= name; test_data; rpc_of; of_rpc}
  in
  {name; description; ty= typ_of}

let abstract ?name ?(description = []) ?test_data of_other to_other def_of_other
    sexp_of_other other_of_sexp =
  let t =
    abstract_def ?name ~description ?test_data of_other to_other def_of_other
  in
  let t_of_sexp sexp = sexp |> other_of_sexp |> of_other in

  let sexp_of_t t = t |> to_other |> sexp_of_other in
  (t, sexp_of_t, t_of_sexp)

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

open Sexplib.Std

module MakeMap (Ord : OrderedType) = struct
  include Map.Make (Ord)

  type 'a other = (Ord.t * 'a) list [@@deriving rpcty, sexp]

  let of_other other = other |> List.to_seq |> of_seq

  let to_other = bindings

  let t ?name ?description t_of_elt =
    let open Rpc.Types in
    let name =
      Option.value name
        ~default:(Printf.sprintf "map from %s to %s" Ord.t.name t_of_elt.name)
    in
    abstract_def ~name ?description of_other to_other (other t_of_elt.ty)

  let t_of_sexp elt_of_sexp sexp = sexp |> other_of_sexp elt_of_sexp |> of_other

  let sexp_of_t sexp_of_elt t = t |> to_other |> sexp_of_other sexp_of_elt
end

module MakeSet (Ord : OrderedType) = struct
  include Set.Make (Ord)

  type other = Ord.t list [@@deriving rpcty, sexp]

  let of_other = of_list

  let to_other = elements

  let t, sexp_of_t, t_of_sexp =
    let open Rpc.Types in
    let name = Printf.sprintf "set of %s" Ord.t.name in
    abstract ~name of_other to_other other sexp_of_other other_of_sexp

  let typ_of = t.ty
end
