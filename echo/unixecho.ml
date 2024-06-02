open Unix
external accept :
  ?cloexec: bool -> file_descr -> file_descr * sockaddr = "caml1_unix_accept"
external unsafe_read : file_descr -> bytes -> int -> int -> int
   = "caml1_unix_read"
external unsafe_write : file_descr -> bytes -> int -> int -> int
                      = "caml1_unix_write"
external close : file_descr -> unit = "caml1_unix_close"
let read fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.read"
  else unsafe_read fd buf ofs len
let write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.write"
  else unsafe_write fd buf ofs len

let lowlat = ref 0
let yield _ =
  if (Thread.self () |> Thread.id) <> !lowlat then
   Thread.yield ()

let worker socket =  
  lowlat := Thread.self () |> Thread.id;
 (* let (_:int) = Picos_prio.Nice.nice (-19) in*)
  let b = Bytes.make 1 ' ' in
  Picos_prio.Timer.set_sigio socket;
  while true do
    let fd, _client = Unix.accept socket in
    (* how about caml_record_signal? *)
    (*Picos_prio.Timer.set_sigio fd;*)

(*    TODO: caml_record_signal after read/write/etc. and measure that*)
    let n = Unix.read fd b 0 1 in
    let m = Unix.write fd b 0 n in
    assert (m = n);
    Unix.close fd
  done

let busy () =
(*  let (_:int) = Picos_prio.Nice.nice (19) in*)
(*  let q = Queue.create () in*)
(*  while true do
    Queue.push (String.make 200 'x') q;
    if Queue.length q > 10000 then (
      Queue.clear q;
      (* add yield points?
      Thread.yield ()
       *)
      );
  done
*)
  while true do () done



let () =
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle yield);
  Sys.set_signal Sys.sigpoll (Sys.Signal_handle yield); (* TODO: rate limited yield *)
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
