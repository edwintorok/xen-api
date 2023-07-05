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

module Operation = struct
  type !'a t =
    (* TODO: bigstring instead *)
    | Read : (bytes -> int -> int -> 'a) -> 'a t
    | Write : {off: int; len: int} -> unit t
end

module Freer = struct
  type !_ t = Pure : 'a -> 'a t | Blocked : 'x blocked * ('x -> 'a) -> 'a t

  and !'a blocked =
    | Op : 'a Operation.t -> 'a blocked
    | Pair : 'a blocked * 'b blocked -> ('a * 'b) blocked
    | Dynamic : 'x blocked * ('x -> 'a) * ('a -> 'b t) -> 'b blocked

  let ( <@@> ) a b = Pair (a, b)

  let lift op = Blocked (Op op, Fun.id)

  let pure x = Pure x

  (* operations are not recursive, and therefore O(1) *)

  let ( <$> ) : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
   fun f -> function
    | Pure x ->
        Pure (f x)
    | Blocked (ops, app) ->
        Blocked (ops, fun x -> x |> app |> f)

  let pairf f1 f2 (x, y) = (f1 x) (f2 y)

  (* see also https://hackage.haskell.org/package/free-5.2/docs/Control-Applicative-Free-Fast.htm *)
  let ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
   fun af ax ->
    match (af, ax) with
    | Pure f, _ ->
        f <$> ax
    | Blocked (ops, app), Pure x ->
        Blocked (ops, Fun.flip app x)
    | Blocked (opsf, appf), Blocked (opsx, appx) ->
        Blocked (opsf <@@> opsx, pairf appf appx)

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

  (* use sparingly, we won't be able to extract all blocked ops if both LHS and RHS of the bind are blocked *)
  module Monad = struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
     fun m f ->
      match m with
      | Pure p ->
          f p
      | Blocked (op, app) ->
          let dynamic = Dynamic (op, app, f) in
          Blocked (dynamic, Fun.id)
  end
end

type conn = {q: string Queue.t; mutable off: int; addr: Unix.sockaddr}

let write t str =
  let len = String.length str in
  let op = Operation.Write {off= t.off; len} in
  t.off <- t.off + len ;
  Queue.push str t.q;
  Freer.lift op

let read _t buf =
  let cb data off len =
    Bytes.blit data off buf 0 len ;
    len
  in
  let op = Operation.Read cb in
  Freer.lift op

let connect addr = {q= Queue.create (); off= 0; addr}

let run conn t =
  let b = Buffer.create 100 in
  let print str = Buffer.add_string b str in
  Queue.iter print conn.q ;
  let s = Buffer.contents b in
  let run_op (type a) (op : a Operation.t) : a =
    match op with
    | Operation.Read cb ->
      (* TODO *)
      cb (Bytes.create 0) 0 0
    | Operation.Write {off; len} ->
        Printf.printf "%d; off: %d, len: %d\n" (String.length s) off len;
        let s = String.sub s off len in
        print_string s
  in
  let open Freer in
  let rec run_blocked : 'a 'b. 'a blocked -> ('a -> 'b) -> 'b =
    fun (type a b) (blocked : a blocked) (app : a -> b) : b ->
     match blocked with
     | Op op ->
         run_op op |> app
     | Pair (a, b) ->
         (run_blocked a Fun.id, run_blocked b Fun.id) |> app
     | Dynamic (op, app', f) ->
         run_actual (run_blocked op app' |> f) |> app
  and run_actual : 'a. 'a t -> 'a =
    fun (type a) (t : a t) : a ->
     match t with
     | Pure x ->
         x
     | Blocked (blocked, app) ->
         run_blocked blocked app
  in
  run_actual t
