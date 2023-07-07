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

module Check (G : Free) = struct
  let gen = Crowbar.(map [G.Gen.gen G.Gen.gena] G.lift)

  let wrap run t = Rresult.R.(t >>= fun t -> Rresult.R.trap_exn run t)

  let run2 t = t |> G.run |> G.run

  let run = G.run

  let check_eq run x y =
    (* TODO: wrap with result *)
    let x' = wrap run x and y' = wrap run y in
    let pp = Rresult.R.pp ~ok:G.Gen.pp ~error:Rresult.R.pp_exn_trap in
    let error (`Exn_trap (e1, _)) (`Exn_trap (e2, _)) = e1 = e2 in
    let eq = Result.equal ~ok:G.Gen.eq ~error in
    Crowbar.check_eq ~pp ~eq x' y'

  let check1 run ~name f1 f2 =
    Crowbar.(add_test ~name [gen]) @@ fun x ->
    check_eq run (Rresult.R.trap_exn f1 x) (Rresult.R.trap_exn f2 x)

  let check2 run ~name f1 f2 =
    Crowbar.(add_test ~name [gen; gen]) @@ fun x y ->
    check_eq run
      (() |> Rresult.R.trap_exn @@ fun () -> f1 x y)
      (() |> Rresult.R.trap_exn @@ fun () -> f2 x y)
end

let ( <.> ) f g x = f (g x)

let test_functor (module F : Free) =
  let module C = Check (F) in
  C.check1 C.run ~name:"fmap id" (F.fmap Fun.id) Fun.id ;

  (* usually checking the 2nd law is redundant, but exceptions can make a difference *)
  let g = Fun.id in
  let h _ = failwith "Test" in
  C.check1 C.run ~name:"fmap (g . h) = (fmap g) . (fmap h)"
    (F.fmap @@ g <.> h)
    (F.fmap g <.> F.fmap h)

let const x _ = x

let test_functor_ops (module F : FunctorOps) =
  let module C = Check (F) in
  test_functor (module F) ;
  let open F in
  (* C.check2 ~name:"(<$) = fmap . const"
     (fun x y -> x <$ y)
     (fun x y -> (fmap <.> const) x y) ; *)
  let g = Fun.id in
  C.check1 C.run ~name:"fmap and (<$>)" (fun x -> fmap g x) (fun x -> g <$> x)

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

let test_applicative (module A : Applicative) =
  let module C = Check (A) in
  test_functor (module A) ;
  let open A in
  let f = Fun.id in
  (* TODO: some more interesting 'f', perhaps provided by Gen itself,
       or if we use these in functor form then we have more control over what ['a] can be
     ? *)
  let g = Fun.id in
  C.check1 C.run ~name:"identity: pure id <*> v = v"
    (fun v -> A.pure Fun.id <*> v)
    Fun.id ;
  C.check1 C.run2 ~name:"homomorphism: pure f <*> pure x = pure (f x)"
    (fun x -> pure f <*> pure x)
    (fun x -> pure (f x)) ;
  C.check1 C.run2 ~name:"interchange: u <*> pure y = pure (\\f -> f y) <*> u"
    (fun y ->
      let u = pure f in
      u <*> pure y
    )
    (fun y ->
      let u = pure f in
      pure (fun f -> f y) <*> u
    ) ;
  C.check1 C.run
    ~name:"composition: u <*> (v <*> w) = pure (.) <*> u <*> v <*> w"
    (fun w ->
      let u = pure f and v = pure f in
      u <*> (v <*> w)
    )
    (fun w ->
      let u = pure f and v = pure f in
      pure ( <.> ) <*> u <*> v <*> w
    ) ;
  C.check1 C.run ~name:"relation to functor: fmap g x = pure g <*> x"
    (fun x -> fmap g x)
    (fun x -> pure g <*> x)

let test_applicative_ops (module A : ApplicativeOps) =
  let module C = Check (A) in
  test_applicative (module A) ;
  test_functor_ops (module A) ;
  let open A in
  let liftA2 f x y = f <$> x <*> y in
  C.check2 C.run ~name:"a1 (*>) a2 = (id <$ a1) <*> a2"
    (fun a1 a2 -> a1 *> a2)
    (fun a1 a2 -> Fun.id <$ a1 <*> a2) ;
  C.check2 C.run ~name:"(<*) = liftA2 const"
    (fun x y -> x <* y)
    (fun x y -> liftA2 const x y)

module type Monad = sig
  include Applicative

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MonadOps = sig
  include Monad

  val ( >> ) : _ t -> 'a t -> 'a t
end

let test_monad (module M : Monad) =
  let module C = Check (M) in
  test_applicative (module M) ;
  let open M in
  let join x = x >>= Fun.id in
  let k = return in
  let h = return in
  let f = Fun.id in
  (*  C.check1 ~name:"return a >>= k = k a" (fun a -> return a >>= k) (fun a -> k a) ; *)
  C.check1 C.run ~name:"m >>= return = m" (fun m -> m >>= return) (fun m -> m) ;
  C.check1 C.run ~name:"m >>= (fun x -> k x >>= h) = (m >>= k) >>= h"
    (fun m -> m >>= fun x -> k x >>= h)
    (fun m -> m >>= k >>= h) ;
  C.check1 C.run2 ~name:"join . fmap join = join . join"
    (fun x -> pure (pure (pure x)) |> (join <.> fmap join))
    (fun x -> pure (pure (pure x)) |> (join <.> join)) ;
  C.check1 C.run ~name:"join . fmap return = join . return = id"
    (join <.> fmap return)
    (join <.> return) ;
  C.check1 C.run ~name:"join . return = id" (join <.> return) Fun.id ;
  C.check1 C.run2 ~name:"return . f = fmap f . return" (return <.> f)
    (fmap f <.> return) ;
  C.check1 C.run2 ~name:"join . fmap (fmap f) = fmap f . join"
    (fun x -> pure (pure x) |> (join <.> fmap (fmap f)))
    (fun x -> pure (pure x) |> (fmap f <.> join))

let test_monad_ops (module M : MonadOps) =
  let module C = Check (M) in
  test_monad (module M) ;
  let open M in
  C.check2 C.run ~name:"m >> n = m >>= \\_ -> n"
    (fun m n -> m >> n)
    (fun m n -> m >>= fun _ -> n)
