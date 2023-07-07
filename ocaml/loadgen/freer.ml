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

module Make (O : sig
  type !'a t

  (* hack for testing TODO *)
  val run : 'a t -> 'a
end) =
struct
  module T = O

  type +!'a t = Pure : 'a -> 'a t | Blocked : 'x op * ('x -> 'a) -> 'a t

  and !'a op =
    | Op : 'a O.t -> 'a op
    | Pair : 'a op * 'b op -> ('a * 'b) op
    | Dynamic : 'x op * ('x -> 'a) * ('a -> 'b t) -> 'b op

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

  let ( *> ) a1 a2 = Fun.id <$ a1 <*> a2

  let ( <* ) a b = liftA2 const a b

  let return = pure

  let ( >>= ) m f =
    match m with
    | Pure x ->
        f x
    | Blocked (op, map) ->
        let dynamic = Dynamic (op, map, f) in
        Blocked (dynamic, Fun.id)

  let ( >>> ) a b =
    (* TODO: optimized version from speculative.ml *)
    a >>= fun () -> b

  let ( >> ) m n = ignore <$> m >>> n

  let rec run_op : 'a. 'a op -> 'a =
    fun (type a) (op : a op) : a ->
     match op with
     | Op op ->
         O.run op
     | Pair (x, y) ->
         (run_op x, run_op y)
     | Dynamic (op, app, f) ->
         run (run_op op |> app |> f)

  (* TODO: batching/etc runner *)
  and run : 'a. 'a t -> 'a =
    fun (type a) (t : a t) : a ->
     match t with Pure x -> x | Blocked (op, app) -> run_op op |> app
end
