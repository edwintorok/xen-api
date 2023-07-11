let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let rec connect_start sock addr =
  let open Unix in
  try connect sock addr with
  | Unix_error ((EINPROGRESS | EAGAIN | EWOULDBLOCK), _, _) ->
      ()
  | Unix_error (EINTR, _, _) ->
      (connect_start [@tailcall]) sock addr

let responses = ref 0

module Connection = struct
  type t = {
      addr: Unix.sockaddr
    ; socket: Unix.file_descr
    ; send_buffer: string Queue.t
    ; zb: Zero_http.Zero_buffer.t
    ; parser: Zero_http.Response.t
    ; mutable closed: bool
    ; mutable off: int
    ; mutable len: int
  }

  let invalid_buf = Bigstringaf.create 1

  let invalid_zb = Zero_http.Zero_buffer.of_bigstring invalid_buf ~off:0 ~len:0

  let invalid =
    let socket = Unix.stdin in
    {
      addr= Unix.ADDR_INET (Unix.inet_addr_any, 0)
    ; socket
    ; send_buffer= Queue.create ()
    ; closed= true
    ; off= 0
    ; len= 0
    ; zb= invalid_zb
    ; parser=
        Zero_http.Response.create invalid_zb (fun _ -> failwith "invalid") ()
    }

  let rec refill socket ~off ~len into =
    match Bigstring_unix.read socket ~off ~len into with
    | 0 ->
        0
    | nread ->
        nread
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        (refill [@tailcall]) socket ~off ~len into
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        -1

  let connect addr =
    let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    try
      Unix.setsockopt socket Unix.TCP_NODELAY true ;
      Unix.set_nonblock socket ;
      (* TODO: share slice of global send_receive_buffer *)
      let buf = Bigstringaf.create 8192 in
      let zb =
        Zero_http.Zero_buffer.of_bigstring buf ~off:0
          ~len:(Bigstringaf.length buf)
      in
      let parser = Zero_http.Response.create zb refill socket in
      connect_start socket addr ;
      {
        addr
      ; send_buffer= Queue.create ()
      ; closed= false
      ; off= 0
      ; len= 0
      ; socket
      ; parser
      ; zb
      }
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Unix.close socket ;
      Printexc.raise_with_backtrace e bt

  let disconnect t = t.closed <- true

  let write conn str =
    assert (not conn.closed) ;
    Queue.push str conn.send_buffer

  let flush _conn = ()

  let sum acc s = acc + String.length s

  let sum_queued_bytes acc t = Queue.fold sum acc t.send_buffer

  let serialize into off t =
    let dst_off = ref off in
    let serialize_entry str =
      let len = String.length str in
      Bigstringaf.blit_from_string str ~src_off:0 into ~dst_off:!dst_off ~len ;
      dst_off := !dst_off + len
    in
    Queue.iter serialize_entry t.send_buffer ;
    t.off <- off ;
    t.len <- !dst_off - off ;
    Queue.clear t.send_buffer ;
    !dst_off

  let rec send t buffer =
    if t.len > 0 then
      try
        assert (not t.closed) ;
        let written =
          Bigstring_unix.write t.socket ~off:t.off ~len:t.len buffer
        in
        t.off <- t.off + written ;
        t.len <- t.len - written ;
        written
      with Unix.(Unix_error (EINTR, _, _)) -> (send [@tailcall]) t buffer
    else
      0

  let receive t =
    (* TODO: 'a param so we don't have to allocate *)
    Zero_http.Response.read t.parser
      (fun ~status_code ~content_length:_ ~headers_size:_ ->
        if status_code = 200 || status_code = 403 then
          incr responses
    )
end

type t = {
    mutable send_receive_buffer: Bigstringaf.t
  ; mutable receive_off: int
  ; mutable connections: Connection.t list
  ; mutable conntable: Connection.t array
  ; mutable conns: int
}

let init () =
  {
    send_receive_buffer= Bigstringaf.create 8192 (* will get resized *)
  ; connections= []
  ; conntable= [||]
  ; receive_off= -1
  ; conns= 0
  }

let connect t addr =
  let conn = Connection.connect addr in
  t.connections <- conn :: t.connections ;
  t.conns <- t.conns + 1 ;
  conn

let ensure_buffer t size =
  if Bigstringaf.length t.send_receive_buffer < size then
    t.send_receive_buffer <- Bigstringaf.create size

let timeout_ms = 30_000

let do_disconnect t mux conn =
  assert conn.Connection.closed ;
  Polly.del mux conn.Connection.socket ;
  Unix.close conn.socket ;
  Queue.clear conn.send_buffer ;
  t.conns <- t.conns - 1

let fastpath_handle_event mux fd events t =
  let conn = t.conntable.(Xapi_stdext_unix.Unixext.int_of_file_descr fd) in
  if Polly.Events.(test events out) then
    if Connection.send conn t.send_receive_buffer = 0 then (
      Unix.shutdown fd Unix.SHUTDOWN_SEND ;
      Polly.upd mux fd Polly.Events.inp
    ) ;
  if Polly.Events.(test events inp) then (
    Connection.receive conn ;
    if Zero_http.Zero_buffer.is_eof conn.zb then begin
      Connection.disconnect conn ;
      do_disconnect t mux conn
    end
  ) ;
  (* TODO: handle errors *)
  t

let rec fastpath mux ~connections t =
  if t.conns > 0 then
    let (_ : t) =
      Polly.wait_fold mux connections timeout_ms t fastpath_handle_event
    in
    fastpath mux ~connections t
  else (* TODO: count responses after decoding them *)
    !responses

let build_conntable connections =
  let conns =
    connections
    |> List.to_seq
    |> Seq.map (fun t ->
           Xapi_stdext_unix.Unixext.int_of_file_descr t.Connection.socket
       )
  in
  let maxfd = Seq.fold_left max 0 conns + 1 in
  let conntbl = Array.make maxfd Connection.invalid in
  List.iter
    (fun conn ->
      conntbl.(Xapi_stdext_unix.Unixext.int_of_file_descr conn.Connection.socket) <-
        conn
    )
    connections ;
  conntbl

let repeat = 100

let nconn = 100

let run ?(receive_buffer_size = 16384) t =
  let connections = List.length t.connections in
  (* we need a fast way to look up Connection.t given an fd: a hashtable would be too slow *)
  t.conntable <- build_conntable t.connections ;
  let need_write_buffer =
    List.fold_left Connection.sum_queued_bytes 0 t.connections
  in
  let need_receive_buffer = connections * repeat * receive_buffer_size in
  ensure_buffer t (need_write_buffer + need_receive_buffer) ;
  let off =
    List.fold_left (Connection.serialize t.send_receive_buffer) 0 t.connections
  in
  t.receive_off <- off ;
  (* off is receive offset *)
  let mux = Polly.create () in
  let register_connection conn =
    Polly.add mux conn.Connection.socket Polly.Events.(inp lor out)
  in
  let finally () = Polly.close mux in
  Fun.protect ~finally @@ fun () ->
  List.iter register_connection t.connections ;
  Gc.compact () ;
  (* TODO: wait until all connections are established? *)
  let t0 = Unix.gettimeofday () in
  let requests = fastpath mux ~connections t in
  let t1 = Unix.gettimeofday () in
  List.iter
    (fun conn ->
      if not conn.Connection.closed then (
        Connection.disconnect conn ; do_disconnect t mux conn
      )
    )
    t.connections ;
  Printf.printf "Requests/s: %f\n#Requests: %d\n"
    (float requests /. (t1 -. t0))
    requests

let () =
  let t = init () in
  let addr = (Unix.getaddrinfo "perfuk-18-06d.xenrt.citrite.net" "80" [] |> List.hd).Unix.ai_addr in
  for _ = 1 to nconn do
    let conn = connect t addr in
    for _ = 1 to repeat do
      Connection.write conn "GET / HTTP/1.1\r\nHost: localhost:8000\r\n\r\n"
    done
  done ;
  run t
