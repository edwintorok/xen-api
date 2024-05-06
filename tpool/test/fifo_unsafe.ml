(* This module is not thread-safe *)

type 'a t = 'a Queue.t

let create = Queue.create

let is_empty = Queue.is_empty

let push t x = Queue.push x t

let pop_opt = Queue.take_opt

type 'a cursor = 'a Seq.t

let snapshot = Queue.to_seq

let next = Seq.uncons
