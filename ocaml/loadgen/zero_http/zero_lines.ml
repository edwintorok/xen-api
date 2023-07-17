type t = Z: {
    zb: Zero_buffer.t
  ; read: 'a Zero_buffer.refill
  ; input: 'a
  ; mutable scanned_eol: int
  ; mutable in_call: bool
} -> t

let make zb ~read input = Z {zb; read; input; scanned_eol= 0; in_call = false}

let read_line (Z t) process_line acc input =
  assert (not t.in_call);
  Zero_buffer.refill t.zb t.read t.input ;
  let consumer = Zero_buffer.consumer t.zb in
  if Zero_buffer.View.size consumer > 0 then
  match Zero_buffer.View.memchr consumer ~pos:t.scanned_eol '\n' with
  | -1 ->
      (* no newline yet *)
      t.scanned_eol <- t.scanned_eol + Zero_buffer.View.size consumer ;
      acc
  | pos ->
      t.scanned_eol <- 0;
      (* assert (Zero_buffer.View.get consumer (pos-1) = '\r'); *)
      let eol_len = pos - 1 in
      Zero_buffer.View.debug consumer ~len:eol_len;
      t.in_call <- true;
      (* TODO: exceptions *)
      let acc = process_line acc input consumer ~eol_len in
      t.in_call <- false;
      Zero_buffer.consumed t.zb (pos+1);
      acc
  else acc

let read_data (Z t) callback input =
    assert (not t.in_call);
    Zero_buffer.refill t.zb t.read t.input;
    let consumer = Zero_buffer.consumer t.zb in
    (* Zero_buffer.View.debug consumer ~len:(Zero_buffer.View.size consumer); *)
    if Zero_buffer.View.size consumer > 0 then begin
        t.in_call <- true;
        (* TODO: exceptions *)
        let nread = callback input consumer in
        t.in_call <- false;
        Zero_buffer.consumed t.zb nread;
        nread > 0
    end else false