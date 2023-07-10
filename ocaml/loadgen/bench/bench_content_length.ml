open Bechamel

let test_data_str =
  String.concat "\r\n"
    [
      "HTTP/1.1 200 OK"
    ; "Server: nginx/1.24.0"
    ; "Date: Fri, 07 Jul 2023 10:39:34 GMT"
    ; "Content-Type: text/html"
    ; "Content-Length: 8474"
    ; "Last-Modified: Mon, 20 Feb 2023 00:00:00 GMT"
    ; "Connection: keep-alive"
    ; {|ETag: "63f2b800-211a"|}
    ; "Accept-Ranges: bytes"
    ; ""
    ; ""
    ]

let test_data =
  Bigstringaf.of_string ~off:0 ~len:(String.length test_data_str) test_data_str

let rec line_memchr buff f line_off off acc =
  match Bigstringaf.memchr buff off '\r' (Bigstringaf.length buff - off) with
  | -1 ->
      acc
  | pos ->
      if Bigstringaf.get buff (pos + 1) = '\n' then
        let acc = f acc buff line_off (pos - off) in
        let line_off = pos + 2 in
        (line_memchr [@tailcall]) buff f line_off line_off acc
      else
        line_memchr buff f line_off (pos + 1) acc

let line_memchr buff f () = line_memchr buff f 0 0 0

let line_split buff f () =
  let s = Bigstringaf.to_string buff in
  let lines = String.split_on_char '\n' s in
  let f acc line = f acc line 0 (String.length line) in
  List.fold_left f 0 lines - 1 (* extra empty line *)

module OCamllex = struct
  let allocate buff =
    let off = ref 0 in
    let lexbuf =
      (* with_positions (the default would allocate) *)
      Lexing.from_function ~with_positions:false @@ fun dst len ->
      let len = Int.min len (Bigstringaf.length buff - !off) in
      if len > 0 then (
        Bigstringaf.blit_to_bytes buff ~src_off:!off dst ~dst_off:0 ~len ;
        off := !off + len
      ) ;
      len
    in
    (buff, lexbuf, off)

  let reset (_, lexbuf, off) =
    off := 0 ;
    Lexing.flush_input lexbuf

  let line_ocamllex buff f (buff', lexbuf, _) =
    assert (buff' == buff) ;
    let code = Find_content_length.status_line lexbuf in
    let content_length = Find_content_length.content_length_header lexbuf in
    let headers_end = Find_content_length.ignore_headers lexbuf in
    f code content_length headers_end
end

module Regex = struct
  let re =
    Re.Posix.compile_pat
      "^HTTP/1.1 ([0-9]{3}) [^\r]*\r\n\
       ([^\r]+\r\n\
       )*[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:[ \t]*([0-9]+)[ \
       \t]*\r\n\
       ([^\r]+\r\n\
       )*\r\n"

  let allocate buff = Bigstringaf.to_string buff

  let digit str pos =
    let c = str.[pos] in
    let n = Char.code c - Char.code '0' in
    assert (n >= 0 && n < 10) ;
    n

  let rec number str ~finish ~start acc =
    if start < finish then
      number str ~finish ~start:(start + 1) @@ ((acc * 10) + digit str start)
    else
      acc

  let line_regex _buff f str =
    let groups = Re.exec re str in
    if Re.Group.test groups 1 && Re.Group.test groups 3 then
      let status =
        number str ~finish:(Re.Group.stop groups 1)
          ~start:(Re.Group.start groups 1) 0
      in
      let len =
        number str ~finish:(Re.Group.stop groups 3)
          ~start:(Re.Group.start groups 3) 0
      in
      f status len (Re.Group.stop groups 0)
end

let rec line_bigstringaf_get buff f line_off acc len off =
  if off < len then
    let c = Bigstringaf.unsafe_get buff off in
    if c = '\r' && Bigstringaf.unsafe_get buff (off + 1) = '\n' then
      let acc = f acc buff line_off (off - line_off) in
      line_bigstringaf_get buff f (off + 1) acc len (off + 1)
    else
      line_bigstringaf_get buff f line_off acc len (off + 1)
  else
    acc

let line_bigstringaf_get buff f () =
  line_bigstringaf_get buff f 0 0 (Bigstringaf.length buff) 0

let repeat = 100

let check_headers code content_length headers_end =
  (* Printf.printf "code: %d, content_length: %d, headers_end: %d; %d\n" code content_length headers_end (String.length test_data_str); *)
  assert (code = 200) ;
  assert (content_length = 8474) ;
  assert (headers_end = String.length test_data_str)

let test_line name ~allocate ~reset f =
  let t = allocate test_data in
  Test.make ~name
  @@ Staged.stage
  @@ fun () ->
  for _ = 1 to repeat do
    reset t ;
    f test_data check_headers t
  done

module RIO = struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) m f = f m

  type ic = {buf: Bigstringaf.t; mutable off: int}

  type oc = unit

  type conn = unit

  let write _ = failwith "response-w"

  let flush _ = failwith "response-f"

  let read_line ic =
    let len = Bigstringaf.length ic.buf - ic.off in
    if len = 0 then
      None
    else
      match Bigstringaf.memchr ic.buf ic.off '\r' len with
      | -1 ->
          let off = ic.off in
          ic.off <- ic.off + len ;
          Some (Bigstringaf.substring ic.buf ~off ~len)
      | pos ->
          if Bigstringaf.get ic.buf (pos + 1) = '\n' then (
            let off = ic.off in
            ic.off <- pos + 2 ;
            Some (Bigstringaf.substring ic.buf ~off ~len:(pos - off))
          ) else
            failwith "TODO: bad line"

  let read _ = failwith "TODO"
end

module R = Cohttp.Response.Make (RIO)

let cohttp_allocate buf = RIO.{buf; off= 0}

let cohttp_reset t = t.RIO.off <- 0

let cohttp_response _ f t =
  match R.read t with
  | `Eof ->
      failwith "EOF"
  | `Invalid str ->
      invalid_arg str
  | `Ok r ->
      let h = Cohttp.Response.headers r in
      let len =
        Cohttp.Header.get h "Content-Length" |> Option.get |> int_of_string
      in
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status r) in
      f status len t.RIO.off

let error_handler _ = failwith "error"

let httpaf_allocate _buff =
  (* can only parse if we have sent a request.. *)
  Httpaf.Request.create `GET "/"

let httpaf_reset _ = ()

let rec httpaf_response buff f ((req, _) as t) ~off ~len =
  match Httpaf.Client_connection.next_read_operation req with
  | `Close ->
      off
  | `Read ->
      if len = 0 then (
        let n = Httpaf.Client_connection.read_eof req buff ~off ~len:0 in
        assert (n = 0) ;
        httpaf_response buff f t ~off ~len
      ) else
        let consumed = Httpaf.Client_connection.read req buff ~off ~len in
        httpaf_response buff f t ~off:(off + consumed) ~len:(len - consumed)

let httpaf_response buff f req =
  let resp = ref None in
  let response_handler r _ = resp := Some r in
  let _, req =
    Httpaf.Client_connection.request req ~error_handler ~response_handler
  in
  let headers_end =
    httpaf_response buff f (req, resp) ~off:0 ~len:(Bigstringaf.length buff)
  in
  match !resp with
  | None ->
      failwith "No response"
  | Some resp -> (
    match Httpaf.Response.body_length ~request_method:`GET resp with
    | `Fixed len ->
        f (Httpaf.Status.to_code resp.status) (Int64.to_int len) headers_end
    | _ ->
        failwith "bad body length"
  )

module StateParser = struct
  type state = WaitEndOfHeaders | Discard | Invalid

  type t = {
      buff: Bigstringaf.t
    ; mutable off: int
    ; mutable len: int
    ; mutable eoh_off: int
    ; mutable state: state
    ; mutable discard: int
    ; mutable content_length: int
  }

  let allocate buff =
    let len = Bigstringaf.length buff in
    {
      buff
    ; off= 0
    ; len
    ; state= WaitEndOfHeaders
    ; eoh_off= 0
    ; discard= 0
    ; content_length= 0
    }

  let reset t =
    t.state <- WaitEndOfHeaders ;
    t.eoh_off <- 0

  let http_200 = "HTTP/1.1 200 "

  let rec skip_to_next_line t buff off =
    match
      Bigstringaf.memchr buff off '\r' (Bigstringaf.length buff - off - 1)
    with
    | -1 ->
        -1
    | pos ->
        let pos = pos + 1 in
        if Bigstringaf.unsafe_get buff pos = '\n' then
          pos + 1
        else
          skip_to_next_line t buff pos

  let content_length = "Content-Length: "

  let invalid t =
    t.state <- Invalid ;
    false

  let discard t =
    let len = Int.min t.discard (t.len - t.off) in
    assert (len >= 0) ;
    if len > 0 then (
      t.off <- t.off + len ;
      t.len <- t.len - len ;
      t.discard <- t.discard - len
    ) ;
    if t.discard = 0 then (
      t.state <- WaitEndOfHeaders ;
      true
    ) else
      false

  let rec parse_content_length_value t buff off acc =
    let c = Bigstringaf.unsafe_get buff off in
    if c = '\r' then (
      t.off <- t.eoh_off ;
      t.discard <- acc ;
      t.content_length <- acc ;
      t.state <- Discard ;
      discard t
    ) else
      let n = Char.code c - Char.code '0' in
      assert (n >= 0 && n < 10) ;
      parse_content_length_value t buff (off + 1) @@ ((acc * 10) + n)

  let rec parse_headers t buff off =
    if
      Bigstringaf.memcmp_string buff off content_length 0
        (String.length content_length)
      = 0
    then
      parse_content_length_value t buff (off + String.length content_length) 0
    else
      parse_headers t buff @@ skip_to_next_line t buff off

  let parse_status_line t buff =
    (* headers to parse between t.off and t.eof_off *)
    if
      Bigstringaf.memcmp_string buff t.off http_200 0 (String.length http_200)
      = 0
    then
      let off = skip_to_next_line t buff (t.off + String.length http_200) in
      parse_headers t buff off
    else
      invalid t

  let end_of_headers = Bigstringaf.of_string "\r\n\r\n" ~off:0 ~len:4

  let rec wait_end_of_headers buff t =
     let len = t.len - t.eoh_off - 3 in
    match Bigstringaf.memchr buff t.eoh_off '\r' len with
     | -1 ->
         t.eoh_off <- t.eoh_off + len ;
         (* wait for more data, haven't encountered end of headers yet *)
         false
     | pos ->
        (* check 2nd eol first, it'll likely filter out false hits sooner *)
        if
          Bigstringaf.unsafe_get_int16_le buff (pos + 2) = 0x0a0d
          && Bigstringaf.unsafe_get_int16_le buff pos = 0x0a0d
        then (
          t.eoh_off <- pos + 3 ;
          parse_status_line t buff
        ) else (
          t.eoh_off <- pos + 1 ;
          (* this was the end of one header, not of all headers, keep searching *)
          wait_end_of_headers buff t
        )

  let parse buff f t =
    assert (buff == t.buff) ;
    let finished =
      match t.state with
      | WaitEndOfHeaders ->
          wait_end_of_headers buff t
      | Discard ->
          discard t
      | Invalid ->
          failwith "Parsing failed" (* TODO: fall back *)
    in
    if finished then
      f 200 t.content_length t.eoh_off
end
module StateParserMM = struct
  type state = WaitEndOfHeaders | Discard | Invalid

  type t = {
      buff: Bigstringaf.t
    ; mutable off: int
    ; mutable len: int
    ; mutable eoh_off: int
    ; mutable state: state
    ; mutable discard: int
    ; mutable content_length: int
  }

  let allocate buff =
    let len = Bigstringaf.length buff in
    {
      buff
    ; off= 0
    ; len
    ; state= WaitEndOfHeaders
    ; eoh_off= 0
    ; discard= 0
    ; content_length= 0
    }

  let reset t =
    t.state <- WaitEndOfHeaders ;
    t.eoh_off <- 0

    (* TODO: duplicate stateparser and stateparser memem so we can compare side by side *)
  let memmem buff off len needle =
    (* TODO: assert bounds checks *)
    Base_bigstring.unsafe_memmem ~haystack:buff ~needle ~haystack_pos:off
      ~haystack_len:len
      ~needle_pos:0
      ~needle_len:(Bigstringaf.length needle)
    (*match Base_bigstring.memmem ~haystack:buff ~needle ~haystack_pos:off
      ~haystack_len:len
      ~needle_pos:0
      ~needle_len:(Bigstringaf.length needle) () with
    | None -> -1
    | Some pos -> pos *)


  let http_200 = "HTTP/1.1 200 "

  let rec skip_to_next_line t buff off =
    match
      Bigstringaf.memchr buff off '\r' (Bigstringaf.length buff - off - 1)
    with
    | -1 ->
        -1
    | pos ->
        let pos = pos + 1 in
        if Bigstringaf.unsafe_get buff pos = '\n' then
          pos + 1
        else
          skip_to_next_line t buff pos

  let content_length = "Content-Length: "
  let content_length = Bigstringaf.of_string content_length ~off:0 ~len:(String.length content_length)

  let invalid t =
    t.state <- Invalid ;
    false

  let discard t =
    let len = Int.min t.discard (t.len - t.off) in
    assert (len >= 0) ;
    if len > 0 then (
      t.off <- t.off + len ;
      t.len <- t.len - len ;
      t.discard <- t.discard - len
    ) ;
    if t.discard = 0 then (
      t.state <- WaitEndOfHeaders ;
      true
    ) else
      false

  let rec parse_content_length_value t buff off acc =
    let c = Bigstringaf.unsafe_get buff off in
    if c = '\r' then (
      t.off <- t.eoh_off ;
      t.discard <- acc ;
      t.content_length <- acc ;
      t.state <- Discard ;
      discard t
    ) else
      let n = Char.code c - Char.code '0' in
      assert (n >= 0 && n < 10) ;
      parse_content_length_value t buff (off + 1) @@ ((acc * 10) + n)

  let parse_headers t buff off =
    match memmem buff off (Bigstringaf.length buff - off) content_length with
    | -1 -> invalid t
    | off ->
      parse_content_length_value t buff (off + Bigstringaf.length content_length) 0

  let parse_status_line t buff =
    (* headers to parse between t.off and t.eof_off *)
    if
      Bigstringaf.memcmp_string buff t.off http_200 0 (String.length http_200)
      = 0
    then
      let off = skip_to_next_line t buff (t.off + String.length http_200) in
      parse_headers t buff off
    else
      invalid t

  let end_of_headers = Bigstringaf.of_string "\r\n\r\n" ~off:0 ~len:4

  let rec wait_end_of_headers buff t =
    let len = t.len - t.eoh_off - 3 in
    match memmem buff t.eoh_off len end_of_headers with
    | -1 ->
        t.eoh_off <- t.eoh_off + len ;
        (* wait for more data, haven't encountered end of headers yet *)
        false
    | pos ->
        t.eoh_off <- pos + 3 ;
        parse_status_line t buff

  let parse buff f t =
    assert (buff == t.buff) ;
    let finished =
      match t.state with
      | WaitEndOfHeaders ->
          wait_end_of_headers buff t
      | Discard ->
          discard t
      | Invalid ->
          failwith "Parsing failed" (* TODO: fall back *)
    in
    if finished then
      f 200 t.content_length t.eoh_off
end


let benchmarks =
  Test.make_grouped ~name:"content-length"
    [
     test_line ~allocate:Regex.allocate ~reset:ignore "re" Regex.line_regex
    ; test_line ~allocate:OCamllex.allocate ~reset:OCamllex.reset "ocamllex"
        OCamllex.line_ocamllex
    ; test_line ~allocate:cohttp_allocate ~reset:cohttp_reset "cohttp"
        cohttp_response
    ; test_line ~allocate:httpaf_allocate ~reset:httpaf_reset "httpaf"
        httpaf_response;
     test_line ~allocate:StateParser.allocate ~reset:StateParser.reset
        "stateparser" StateParser.parse
    ; test_line ~allocate:StateParserMM.allocate ~reset:StateParserMM.reset
        "stateparser-memmem" StateParserMM.parse
      (* TODO: a fully manual state machine.... from speculative.ml *)
      (* ; test_line ~allocate:ignore ~reset:ignore "memchr" line_memchr
         ; test_line ~allocate:ignore ~reset:ignore "split" line_split
         ; test_line ~allocate:ignore ~reset:ignore "bigstringaf.get"
             line_bigstringaf_get *)
    ]

let () =
  Memtrace.trace_if_requested () ;
  Bechamel_simple_cli.cli benchmarks
