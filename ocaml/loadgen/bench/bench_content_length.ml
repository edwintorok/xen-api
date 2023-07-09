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
    off := 0; Lexing.flush_input lexbuf

  let line_ocamllex buff f (buff', lexbuf, _) =
    assert (buff' == buff) ;
    let code = Find_content_length.status_line lexbuf in
    let content_length = Find_content_length.content_length_header lexbuf in
    let headers_end = Find_content_length.ignore_headers lexbuf in
    f code content_length headers_end
end

module Regex = struct
  let re = Re.Posix.compile_pat "[^\r]*\r\n"

  let allocate buff = Bigstringaf.to_string buff

  let rec line_regex buff f str off acc =
    if off >= String.length str then
      acc
    else
      try
        let groups = Re.exec ~pos:off re str in
        if Re.Group.test groups 0 then
          let start = Re.Group.start groups 0 in
          let finish = Re.Group.stop groups 0 in
          line_regex buff f str finish @@ f acc buff start (finish - start)
        else
          acc
      with Not_found -> acc

  let line_regex buff f str = line_regex buff f str 0 0
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
  assert (code = 200);
  assert (content_length = 8474);
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

let benchmarks =
  Test.make_grouped ~name:"content-length"
    [
    (*  test_line ~allocate:Regex.allocate ~reset:ignore "re" Regex.line_regex *)
     test_line ~allocate:OCamllex.allocate ~reset:OCamllex.reset "ocamllex"
        OCamllex.line_ocamllex
   (* ; test_line ~allocate:ignore ~reset:ignore "memchr" line_memchr
    ; test_line ~allocate:ignore ~reset:ignore "split" line_split
    ; test_line ~allocate:ignore ~reset:ignore "bigstringaf.get"
        line_bigstringaf_get *)
    ]

let () =
  Memtrace.trace_if_requested () ;
  Bechamel_simple_cli.cli benchmarks
