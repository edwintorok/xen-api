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

module TA(X: sig type 'a t end) = struct
    type ('a, 'b) t =
      | Leaf: ('a -> 'b X.t) -> ('a, 'b) t
      | Node : ('a, 'x) t * ('x, 'b) t -> ('a, 'b) t

    let tsingleton r = Leaf r

    let snoc t r = Node(t, Leaf r)
    let append t1 t2 = Node(t1, t2)

    type ('a, 'b) viewl =
      | TOne: ('a -> 'b X.t) ->('a, 'b) viewl
      | TM: ('a -> 'x X.t) * ('x, 'b) t -> ('a, 'b) viewl

    let rec go : 'a 'b 'x. ('a, 'x) t -> ('x, 'b) t -> ('a, 'b) viewl =
     fun (type a b x) (t1:(a,x) t) (tr:(x,b) t): (a, b) viewl -> match t1 with
      |Leaf r -> TM (r, tr)
      | Node (tl1, tl2) -> go tl1 (Node(tl2, tr))
      
    let tviewl = function
    | Leaf r -> TOne r      
    | Node (t1, t2) ->
      go t1 t2
end

module Make (O : sig
  type !'a t

  (* hack for testing TODO *)
  val run : 'a t -> 'a
end) =
struct
  module T = O

  (* need zseq: type aligned sequence,
  Freer Monads, more extensible effects.
  Paragraph 3, Freer Applicative: type aligned sequence...
   *)
  
  type +!'a t = Pure : 'a -> 'a t | Blocked : 'x op * ('x -> 'a) -> 'a t

  and !'a op = Op : 'a O.t -> 'a op | Pair : 'a op * 'b op -> ('a * 'b) op

  let lift op = Blocked (Op op, Fun.id)

  let ( <.> ) f g x = f (g x)

  let fmap f = function
    | Pure x ->
        Pure (f x)
    | Blocked (op, map) ->
        Blocked (op, fun x -> x |> map |> f)

  let const x _ = x

  let ( <$> ) = fmap

  let ( <$ ) a b = (fmap <.> const) a b

  let pure x = Pure x

  let ( <@@> ) a b = Pair (a, b)

  let pairf f1 f2 (x, y) = (f1 x) (f2 y)

  let ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
   fun af ax ->
    match (af, ax) with
    | Pure f, _ ->
        f <$> ax
    | Blocked (ops, app), Pure x ->
        Blocked (ops, Fun.flip app x)
    | Blocked (opsf, appf), Blocked (opsx, appx) ->
        Blocked (opsf <@@> opsx, pairf appf appx)

  let liftA2 f x y = f <$> x <*> y

  (* TODO: speculative here? *)
  let ( *> ) a1 a2 = Fun.id <$ a1 <*> a2

  let ( <* ) a b = liftA2 const a b

  let ( >>> ) : unit t -> 'a t -> 'a t =
   fun ignorable next ->
    match (ignorable, next) with
    | Pure (), _ ->
        next
    | Blocked (ops, f), Pure x ->
        Blocked
          ( ops
          , fun result ->
              let () = f result in
              x
          )
    | Blocked (opsf, f), Blocked (opsnext, nextf) ->
        Blocked
          ( opsf <@@> opsnext
          , fun (x, y) ->
              let () = f x in
              nextf y
          )

  let ( >> ) m n = ignore <$> m >>> n

  let rec run_op : 'a. 'a op -> 'a =
    fun (type a) (op : a op) : a ->
     match op with Op op -> O.run op | Pair (x, y) -> (run_op x, run_op y)

  (* TODO: batching/etc runner *)
  and run : 'a. 'a t -> 'a =
    fun (type a) (t : a t) : a ->
     match t with Pure x -> x | Blocked (op, app) -> run_op op |> app
end
