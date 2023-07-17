module Zero_buffer = Zero_buffer

let src = Logs.Src.create ~doc:"zero_http protocol logging" "loadgen.zero_http"

module Log = (val Logs.src_log src)

module Response = struct
  type state = WaitStatusLine | WaitContentLength | WaitEndOfHeaders | Discard

  type t = {
      lines: Zero_lines.t
    ; mutable state: state
    ; mutable status_code: int
    ; mutable content_length: int
    ; mutable discard: int
    ; mutable headers_size: int
  }

  let create zb reader input =
    let lines = Zero_lines.make zb ~read:reader input in
    {
      lines
    ; content_length= 0
    ; state= WaitStatusLine
    ; discard= 0
    ; status_code= 0
    ; headers_size= 0 (* TODO: this is not updated yet *)
    }

  exception Fallback

  let debug_level = Some Logs.Debug

  let is_debug () = Logs.Src.level src = debug_level

  let invalid _t =
    if is_debug () then
      Log.debug (fun m -> m "Cannot parse HTTP line, falling back to slow parser");
    raise Fallback

  let finish t callback =
    (* reset *)
    t.state <- WaitStatusLine ;
    let content_length = t.content_length
    and headers_size = t.headers_size
    and status_code = t.status_code in
    t.status_code <- 0 ;
    t.headers_size <- 0 ;
    t.content_length <- 0 ;

    if is_debug () then
      Log.debug (fun m ->
          m
            "Parsed an HTTP reply: status_code: %d, content_length: %d, \
             headers_size: %d"
            status_code content_length headers_size
      ) ;

    callback ~status_code ~content_length ~headers_size

  let discard_data t view =
    let available = Zero_buffer.View.size view in
    let to_discard = Int.min available t.discard in
    t.discard <- t.discard - to_discard ;
    to_discard

  let rec discard t =
    t.state <- Discard ;
    (* resume from here *)
    if Zero_lines.read_data t.lines discard_data t && t.state = Discard then
      (discard [@tailcall]) t

  let is_empty_line _ () _ ~eol_len = eol_len = 0

  let rec wait_end_of_headers t =
    t.state <- WaitEndOfHeaders ;
    (* resume from here *)
    if Zero_lines.read_line t.lines is_empty_line false () then (
      if is_debug () then
        Log.debug (fun m -> m "Finished parsing header, about to discard %d bytes of body" t.content_length);
      t.discard <- t.content_length ;
      (discard [@tailcall]) t
    ) else
      (wait_end_of_headers [@tailcall]) t

  let rec parse_content_length_value t ~eol_len ~off acc =
    (* TODO: handle some optional whitespace? *)
    if off = eol_len then
      acc
    else
      let c = Zero_buffer.View.get t off in
      let n = Char.code c - Char.code '0' in
      if n < 0 || n > 9 then
        invalid t
      else
        let acc = (acc * 10) + n in
        (parse_content_length_value [@tailcall]) t ~eol_len ~off:(off + 1) acc

  let content_length1 = "Content-Length: "

  let content_length2 = "content-length: "

  let parse_content_length _ () t ~eol_len =
    if Zero_buffer.View.is_prefix t content_length1
      || Zero_buffer.View.is_prefix t content_length2
     then
      (parse_content_length_value [@tailcall]) t ~eol_len
        ~off:(String.length content_length1)
        0
    else
      -2 (* not content-length *)

  let rec wait_content_length t =
    t.state <- WaitContentLength ;
    (* resume from here *)
    match Zero_lines.read_line t.lines parse_content_length (-1) () with
    | -1 ->
        (* eagain *)
        ()
    | -2 ->
        wait_content_length t
    | content_length ->
        assert (content_length >= 0) ;
        if is_debug () then
          Log.debug (fun m -> m "Parsed Content-Length: %d" content_length);
        t.content_length <- content_length ;
        (wait_end_of_headers [@tailcall]) t

  let http_200 = "HTTP/1.1 200 "

  let http_403 = "HTTP/1.1 403 "

  let parse_status_line acc () view ~eol_len:_ =
    if Zero_buffer.View.is_prefix view http_200 then
      200
    else if Zero_buffer.View.is_prefix view http_403 then
      403
    else
      acc

  let wait_status_line t =
    t.state <- WaitStatusLine ;
    (* resume from here *)
    match Zero_lines.read_line t.lines parse_status_line (-1) () with
    | -1 ->
        ()
    | status_code when status_code = 200 || status_code = 403 ->
        t.status_code <- status_code ;
        if is_debug () then
          Log.debug (fun m -> m "Parsed HTTP status code %d" status_code);
        (wait_content_length [@tailcall]) t
    | status_code ->
        if is_debug () then
          Log.debug (fun m -> m "HTTP status code: %d, falling back to slow parser" status_code);
        failwith "TODO: fallback"

  let read t callback =
    let () =
      match t.state with
      | WaitStatusLine ->
          wait_status_line t
      | WaitContentLength ->
          wait_content_length t
      | WaitEndOfHeaders ->
          wait_end_of_headers t
      | Discard ->
          discard t
    in
    if t.state = Discard && t.discard = 0 then
      finish t callback
end
