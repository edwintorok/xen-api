module Response = struct
  type state = WaitStatusLine | WaitContentLength | WaitEndOfHeaders | Discard

  type t = {
      callback:
        status_code:int -> content_length:int -> headers_size:int -> unit
    ; buff: Bigstringaf.t
    ; mutable state: state
    ; mutable consumed_off: int  (** how far parsing has reached *)
    ; mutable eol_scan_off: int  (** how far did we scan for eof *)
    ; mutable eol_off: int
    ; mutable content_length: int
    ; mutable discard: int
    ; mutable headers_size: int
    ; mutable produced_off: int
          (** data between [[0, off)] is ready for parsing *)
  }

  let create buff callback =
    {
      buff
    ; callback
    ; produced_off= 0
    ; consumed_off= 0
    ; eol_scan_off= -1
    ; eol_off = -1
    ; content_length= 0
    ; state= WaitStatusLine
    ; discard= 0
    ; headers_size= 0 (* TODO: this is not updated yet *)
    }

  exception Fallback

  let invalid _t = raise Fallback

  (** [require t reader conn amount] requires at least [amount] bytes to be available for parsing.
    Calls [reader conn] until the required amount of data is available or raises [Fallback] if we cannot fit an entire header into our buffer.
    [reader] will be always called to with the entire free part of the buffer, potentially moving unparsed data to the beginning of the buffer to make room.
    [reader] is allowed to return [-1] to signal EAGAIN.
   *)
  let rec require_read t reader conn amount =
    let available = t.produced_off - t.consumed_off in
    if amount <= available then
      available
    (* nothing to do, we already have the required amount available *)
    else (
      assert (amount <= Bigstringaf.length t.buff) ;
      if t.consumed_off > 0 then (
        (* move data to beginning of buffer *)
        Bigstringaf.blit t.buff ~src_off:t.consumed_off t.buff ~dst_off:0
          ~len:available ;
        t.eol_scan_off <- t.eol_scan_off - t.consumed_off ;
        t.eol_off <- t.eol_off - t.consumed_off;
        t.consumed_off <- 0 ;
        t.produced_off <- available
      ) ;
      let len = Bigstringaf.length t.buff - t.produced_off in
      match reader conn ~off:t.produced_off ~len t.buff with
      | 0 ->
          invalid t (* EOF before reading required amount of data *)
      | -1 ->
          0
      | nread ->
          t.produced_off <- t.produced_off + nread ;
          (require_read [@tailcall]) t reader conn amount
    )

  let rec read_line t reader conn =
    let len = require_read t reader conn (t.eol_scan_off - t.consumed_off + 2) in
    if len = 0 then
      false
    else
      let len = len - 1 in
      assert (len > 0);
      match Bigstringaf.memchr t.buff t.eol_scan_off '\r' len with
      | -1 ->
          (* no newline in the buffer so far *)
          t.eol_scan_off <- t.eol_scan_off + len ;
          (read_line [@tailcall]) t reader conn
      | pos ->
          if Bigstringaf.get t.buff (pos + 1) = '\n' then (
            t.eol_off <- t.eol_scan_off + pos ;
            t.eol_scan_off <- -1;
            true
          ) else
            invalid t

  let read_line t reader conn status =
    if t.eol_scan_off < 0 then
      t.eol_scan_off <- t.consumed_off;
    t.state <- status ;
    read_line t reader conn

  let http_200 = "HTTP/1.1 200 "

  let require t amount =
    let available = t.eol_off - t.consumed_off in
    assert (available >= 0) ;
    if amount > available then invalid t

  let content_length = "Content-Length: "

  let finished t =
    t.callback ~status_code:200 ~content_length:t.content_length
      ~headers_size:t.headers_size ;
    t.state <- WaitStatusLine ;
    true

  let rec discard t reader conn =
    t.state <- Discard ;
    if t.discard = 0 then
      finished t
    else
      let available = t.produced_off - t.consumed_off in
      let to_discard = Int.min available t.discard in
      t.consumed_off <- t.consumed_off + to_discard ;
      t.discard <- t.discard - available ;
      if t.consumed_off = t.produced_off then (
        t.consumed_off <- 0 ;
        t.produced_off <- 0
      ) ;
      if require_read t reader conn 1 = 0 then
        false
      else
        (discard [@tailcall]) t reader conn

  let rec wait_end_of_headers t reader conn =
    let prev_off = t.consumed_off in
    if read_line t reader conn WaitEndOfHeaders then (
      if t.consumed_off = prev_off + 2 then
        (* this was \r\n\r\n, i.e. end of headers  *)
        t.discard <- t.content_length ;
      (discard [@tailcall]) t reader conn
    ) else
      (wait_end_of_headers [@tailcall]) t reader conn

  let rec parse_content_length_value t reader conn acc =
    (* TODO: handle some optional whitespace? *)
    require t 2 ;
    let c = Bigstringaf.get t.buff t.consumed_off in
    t.consumed_off <- t.consumed_off + 1 ;
    if c = '\r' then
      if Bigstringaf.get t.buff t.consumed_off = '\n' then (
        t.consumed_off <- t.consumed_off + 1 ;
        t.content_length <- acc ;
        (wait_end_of_headers [@tailcall]) t reader conn
      ) else
        invalid t
    else
      let n = Char.code c - Char.code '0' in
      if n < 0 || n > 9 then
        invalid t
      else
        let acc = (acc * 10) + n in
        (parse_content_length_value [@tailcall]) t reader conn acc

  let rec wait_content_length t reader conn =
    if read_line t reader conn WaitContentLength then
      let len = String.length content_length in
      if
        t.eol_off - t.consumed_off >= len
        && Bigstringaf.memcmp_string t.buff t.consumed_off content_length 0 len
           = 0
      then (
        t.consumed_off <- t.consumed_off + len ;
        parse_content_length_value t reader conn 0
      ) else
        (wait_content_length [@tailcall]) t reader conn
    else
      false

  let parse_status_line t reader conn =
    let len' = String.length http_200 in
    require t len' ;
    if Bigstringaf.memcmp_string t.buff t.consumed_off http_200 0 len' = 0 then (
      t.consumed_off <- t.eol_off ;
      (wait_content_length [@tailcall]) t reader conn
    ) else
      false

  let wait_status_line t reader conn =
    if read_line t reader conn WaitStatusLine then
      parse_status_line t reader conn
    else
      false

  let read t reader conn =
    match t.state with
    | WaitStatusLine ->
        wait_status_line t reader conn
    | WaitContentLength ->
        wait_content_length t reader conn
    | WaitEndOfHeaders ->
        wait_end_of_headers t reader conn
    | Discard ->
        discard t reader conn
end
