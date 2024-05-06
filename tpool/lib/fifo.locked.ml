type 'a t =
{ q: 'a Queue.t
; m: Mutex.t
}

let create () = { q = Queue.create (); m = Mutex.create () }
let with_mutex t f =
  let finally () = Mutex.unlock t.m in
  Mutex.lock t.m;
  Fun.protect ~finally f

let is_empty t = with_mutex t @@ fun () -> Queue.is_empty t.q

let push t x = with_mutex t @@ fun () -> Queue.push x t.q

let pop_opt t = with_mutex t @@ fun () -> Queue.take_opt t.q

type 'a cursor = 'a Seq.t

let snapshot t =
  with_mutex t @@ fun () ->
  t.q |> Queue.copy |> Queue.to_seq

let next = Seq.uncons
