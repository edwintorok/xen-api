let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let rec connect_start sock addr =
  let open Unix in
  try connect sock addr with
  | Unix_error ((EINPROGRESS | EAGAIN | EWOULDBLOCK), _, _) ->
      ()
  | Unix_error (EINTR, _, _) ->
      (connect_start [@tailcall]) sock addr

module Connection = struct
  type t = {
      addr: Unix.sockaddr
    ; socket: Unix.file_descr
    ; send_buffer: string Queue.t
    ; mutable closed: bool
    ; mutable off: int
    ; mutable len: int
  }

  let invalid =
    let socket = Unix.stdin in
    {
      addr= Unix.ADDR_INET (Unix.inet_addr_any, 0)
    ; socket
    ; send_buffer= Queue.create ()
    ; closed= true
    ; off= 0
    ; len= 0
    }

  let connect addr =
    let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    try
      Unix.setsockopt socket Unix.TCP_NODELAY true ;
      Unix.set_nonblock socket ;
      connect_start socket addr ;
      {
        addr
      ; send_buffer= Queue.create ()
      ; closed= false
      ; off= 0
      ; len= 0
      ; socket
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
    try
      assert (not t.closed) ;
      let written =
        Bigstring_unix.write t.socket ~off:t.off ~len:t.len buffer
      in
      t.off <- t.off + written ;
      t.len <- t.len - written ;
      written
    with Unix.(Unix_error (EINTR, _, _)) -> (send [@tailcall]) t buffer

  let rec receive t into off =
    try
      let len = Bigstringaf.length into - off in
      assert (len > 0) ;
      Bigstring_unix.read t.socket ~off ~len into
    with Unix.(Unix_error (EINTR, _, _)) -> (receive [@tailcall]) t into off
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
    (* read and discard for now, TODO *)
    let nread = Connection.receive conn t.send_receive_buffer t.receive_off in
    assert (nread >= 0) ;
    t.receive_off <- t.receive_off + nread ;
    if nread = 0 then (
      Connection.disconnect conn ; do_disconnect t mux conn
    )
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
    0

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

let repeat = 999

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
  Printf.printf "Requests/s: %f\n" (float requests /. (t1 -. t0))

let () =
  let t = init () in
  let conn = connect t Unix.(ADDR_INET (Unix.inet_addr_loopback, 8000)) in
  for _ = 1 to repeat do
    Connection.write conn "GET / HTTP/1.1\r\nHost: localhost:8000\r\n\r\n"
  done ;
  run t
