type ro = unit

type wo = unit

type 'a refill = 'a -> off:int -> len:int -> Bigstringaf.t -> int

module View = struct
  type 'a t = {
      buf: Bigstringaf.t
    ; mutable start: int  (** inclusive *)
    ; mutable stop: int  (** exclusive *)
  }

  let get t pos =
     assert (pos >= 0);
     Bigstringaf.get t.buf (t.start + pos)

  let size t = t.stop - t.start

  let memchr t ~pos c =
    assert (pos >= 0);
    assert (pos < t.stop);
    let off = t.start + pos in
    match Bigstringaf.memchr t.buf off c (t.stop - off) with
    | -1 ->
        -1
    | off ->
        off - t.start

  let debug t ~len =
    Printf.eprintf "@%d, +%d: %s\n" t.start len @@ Bigstringaf.substring t.buf ~off:t.start ~len

  let is_prefix t prefix =
    let len = String.length prefix in
    len <= size t && Bigstringaf.memcmp_string t.buf t.start prefix 0 len = 0

end

type t = {
    buf: Bigstringaf.t
  ; half: int
  ; producer: wo View.t
  ; consumer: ro View.t
}

let of_bigstring buf ~off ~len =
  assert (off >= 0) ;
  assert (len >= 0) ;
  {
    buf = Bigstringaf.sub buf ~off ~len
  ; producer= View.{buf; start= 0; stop = len}
  ; half = len / 2
  ; consumer= View.{buf; start= 0; stop= 0}
  }

let check_invariant t =
  assert (0 <= t.consumer.start) ;
  assert (t.consumer.start <= t.consumer.stop) ;
  assert (t.consumer.stop = t.producer.start) ;
  assert (t.producer.start <= t.producer.stop) ;
  assert (t.producer.stop = Bigstringaf.length t.buf)

let reset t =
  check_invariant t ;
  t.producer.start <- 0;
  t.consumer.start <- 0 ;
  t.consumer.stop <- 0 ;
  check_invariant t

let producer t = t.producer

let move_to_beginning t =
  let len = View.size t.consumer in
  Bigstringaf.blit t.buf ~src_off:t.consumer.start t.buf ~dst_off:0 ~len ;
  t.consumer.start <- 0 ;
  t.consumer.stop <- len ;
  t.producer.start <- t.consumer.stop

let produced t amount =
  assert (amount >= 0) ;
  t.producer.start <- t.producer.start + amount ;
  t.consumer.stop <- t.producer.start ;
  assert (t.producer.start <= t.producer.stop)

let refill t reader input =
  let producer = t.producer in
  let len = View.size producer in
  let nread = reader input ~off:producer.start ~len t.buf in
  assert (nread >= 0) ;
  assert (nread <= len) ;
  produced t nread

let consumer t = t.consumer

let consumed t amount =
  assert (amount >= 0) ;
  t.consumer.start <- t.consumer.start + amount ;
  let available = t.consumer.stop - t.consumer.start in
  assert (available >= 0) ;
  if available = 0 then (
    (* reclaim unused space at beginning of the buffer *)
    t.consumer.start <- 0 ;
    t.consumer.stop <- 0 ;
    t.producer.start <- 0
  ) else if t.consumer.start >= t.half then
    (* when more than half of the buffer is empty at the beginning: compact any unconsumed entries *)
    move_to_beginning t 
