type 'a t = {
    zb: Zero_buffer.t
  ; read: 'a Zero_buffer.refill
  ; input: 'a
  ; mutable scanned_eol: int
}

let make zb ~read input = {zb; read; input; scanned_eol= 0}

let read_line t process_line acc input =
  Zero_buffer.refill t.zb t.read t.input ;
  let consumer = Zero_buffer.consumer t.zb in
  match Zero_buffer.View.memchr consumer ~pos:t.scanned_eol '\r' with
  | -1 ->
      (* no newline yet *)
      t.scanned_eol <- t.scanned_eol + Zero_buffer.View.size consumer ;
      acc
  | pos ->
      process_line acc input consumer ~eol_len:pos
