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

(** Speculative execution of applicatives

 {b References}
 {ul
 {- Andrey Mokhov, Georgy Lukyanov, Simon Marlow, and Jeremie Dimino. 2019.
 {e {{:https://doi.org/10.1145/3341694} Selective applicative functors}
 }
*)

(**
  An abstract type for an operation. The only requirement is that it is described by a parametrized type.
  It is not necessary to provide a way to 'run' the operation at this stage.
*)
module type Operation = sig
  (** an operation that returns type ['a]. Any parameters for the operation should be stored in the type/variant itself. *)
  type 'a t
end

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

(**
  Builds a Free Functor out of an {!Operation}.
  Functor here is used in the category theory sense (a type with an [fmap] operation),
  and not in the OCaml sense.

  The "Free" construction is used here to separate the Functor,Applicative,Selective/etc. implementation
  from the actual operation implementation, such that you don't have to be concerned with implementing/following the laws for those categories
  while implementing the operations, and supporting new operations is as easy as defining a way to construct them and extending the [Operation.t] variant.
*)
module MakeFree (O : Operation) : Functor with type 'a t = 'a O.t

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module MakeApplicative (F : Functor) : Applicative
