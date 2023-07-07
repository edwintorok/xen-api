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

(**
  Test that modules implementing various category theory concepts actually obey the laws
  associated with that category.

  {b References}
  {ul
  {- {e {{:https://wiki.haskell.org/Typeclassopedia} Haskell Typeclassopedia }}
  }
  
*)

module type Gen = sig
  type 'a t

  type poly_a

  val gena : poly_a Crowbar.gen

  val gen : poly_a Crowbar.gen -> poly_a t Crowbar.gen

  val eq : poly_a -> poly_a -> bool

  val pp : poly_a Fmt.t
end

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type Free = sig
  module T : sig
    type 'a t
  end

  module Gen : Gen with type 'a t = 'a T.t

  include Functor

  val lift : 'a T.t -> 'a t

  val run : 'a t -> 'a
end

module type FunctorOps = sig
  include Free

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <$ ) : 'a -> _ t -> 'a t
end

val test_functor : (module Free) -> unit

val test_functor_ops : (module FunctorOps) -> unit

module type Applicative = sig
  include Free

  val pure : 'a -> 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module type ApplicativeOps = sig
  include Applicative

  include FunctorOps with type 'a t := 'a t

  val ( *> ) : _ t -> 'a t -> 'a t

  val ( <* ) : 'a t -> _ t -> 'a t
end

val test_applicative : (module Applicative) -> unit

val test_applicative_ops : (module ApplicativeOps) -> unit

module type Monad = sig
  include Applicative

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MonadOps = sig
  include Monad

  val ( >> ) : _ t -> 'a t -> 'a t
end

val test_monad : (module Monad) -> unit

val test_monad_ops : (module MonadOps) -> unit
