let worker socket =  
(*  let (_:int) = Picos_prio.Nice.nice (-19) in*)
  let b = Bytes.make 1 ' ' in
  while true do
    let fd, _client = Unix.accept socket in
    let n = Unix.read fd b 0 1 in
    let m = Unix.write fd b 0 n in
    assert (m = n);
    Unix.close fd
  done

let busy () =
(*  let (_:int) = Picos_prio.Nice.nice (19) in*)
  let q = Queue.create () in
  while true do
    Queue.push (String.make 200 'x') q;
    if Queue.length q > 10000 then (
      Queue.clear q;
      (* add yield points?
      Thread.yield ()
       *)
      );

  done


let yield _ = Thread.yield ()

let () =
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle yield);
  let target = Unix.ADDR_UNIX Sys.argv.(1) in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind socket target;
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.listen socket 4096;
  let busy = Array.init 16 (fun _ -> Picos_prio.Timer.thread_create busy ()) in
(*  let busy = Array.init 1 (fun _ -> Thread.create busy ()) in*)
  let listeners = Array.init 1 (fun _ -> Picos_prio.Timer.thread_create worker socket) in
  Array.iter Thread.join listeners;
  Array.iter Thread.join busy
