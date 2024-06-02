open Unix

let io_waiting = Atomic.make 0
let io_possible = Atomic.make 0

let sigpoll_handler (_:int) = Atomic.incr io_possible

let () =
  Sys.set_signal Sys.sigpoll (Sys.Signal_handle sigpoll_handler)

let[@inline always] before_io () =
  Atomic.incr io_waiting

let[@inline always] after_io () =
  Atomic.decr io_waiting;

  (* signals could've been coalesced, or not received in time, fixup so it is not negative *)
  if Atomic.fetch_and_add io_possible ~-1 <= 0 then
    Atomic.incr io_possible

let timer_handler (_: int) =
    if Atomic.get io_possible > 0 (*|| Atomic.get io_waiting > 0 *) then
      Thread.yield ()

let worker socket =  
  let b = Bytes.make 1 ' ' in
  (* TODO: we really want an "after I/O" signal, not before...
  [we could do this in the C stub, just before leave blocking]
   *)
  Picos_prio.Timer.set_sigio socket;
  while true do
    before_io ();
    let fd, _client = Unix.accept socket in
    after_io ();
    Picos_prio.Timer.set_sigio fd;
    before_io ();
    let n = Unix.read fd b 0 1 in
    after_io ();
    before_io ();
    let m = Unix.write fd b 0 n in
    after_io ();
    assert (m = n);
    before_io ();
    Unix.close fd;
    after_io ()
  done

let busy () =
(*  let q = Queue.create () in
  while true do
    Queue.push (String.make 200 'x') q;
    if Queue.length q > 10000 then (
      Queue.clear q;
      );

  done*)
  while true do () done

let uncond_yield (_:int) = Thread.yield ()

let () =
  (* This must yield..., don't override vtalrm yielding or it breaks *)
  Sys.set_signal Sys.sigprof (Sys.Signal_handle timer_handler);
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle uncond_yield);
  let (_:Unix.interval_timer_status) = Unix.setitimer Unix.ITIMER_VIRTUAL Unix.{it_interval = 0.0075; it_value = 0.0075} in
  let target = Unix.ADDR_UNIX Sys.argv.(1) in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind socket target;
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.listen socket 4096;
  let busy = Array.init 16 (fun _ -> Picos_prio.Timer.thread_create busy ()) in
(*  let busy = Array.init 16 (fun _ -> Thread.create busy ()) in*)
  let listeners = Array.init 1 (fun _ -> Thread.create worker socket) in
  Array.iter Thread.join listeners;
  Array.iter Thread.join busy
