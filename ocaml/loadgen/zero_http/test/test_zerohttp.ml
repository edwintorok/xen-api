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

let () =
  let buf = Bigstringaf.create 64 in
  let callback ~status_code ~content_length ~headers_size =
    Printf.printf "Status code: %d, content length: %d, headers size: %d\n"
      status_code content_length headers_size
  in
  let t = Zerohttp.Response.create buf callback in
  let stroff = ref 0 in
  let reader str ~off ~len buf =
    let len = Int.min len (String.length test_data_str - off) in
    if len > 0 then
      Bigstringaf.blit_from_string str ~src_off:!stroff buf ~dst_off:off ~len;
    stroff := !stroff + len;
    len
  in
  let (_:bool) = Zerohttp.Response.read t reader test_data_str in
  ()