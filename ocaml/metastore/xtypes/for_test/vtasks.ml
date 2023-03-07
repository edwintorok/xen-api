type join_once = {mutex: Mutex.t; mutable thread: Thread.t option}

let create f v = {mutex= Mutex.create (); thread= Some (Thread.create f v)}

let with_mutex m f =
  Mutex.lock m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let join_once t =
  with_mutex t.mutex @@ fun () ->
  let () =
    match t.thread with None -> () | Some thread -> Thread.join thread
  in
  t.thread <- None

type 'a t = {
    thread: join_once
  ; result: ('a, Rresult.R.exn_trap) result option Atomic.t
        (* not protected by the mutex in [join_once] to avoid having to
           release/reacquire the mutex while doing Thread.join *)
}

type task = Rpc.t

type context = unit

let v ~__context:_ typ_of f =
  let call () =
    f () |> Rpcmarshal.unmarshal typ_of
    |> Rresult.R.failwith_error_msg
  in
  let thread result =
    Rresult.R.trap_exn call ()
    |> Option.some |> Atomic.set result
  in
  let result = Atomic.make None in
  {thread= create thread result; result}

let await_exn t =
  join_once t.thread ;
  match Atomic.get t.result with
  | None ->
      assert false (* joined thread must have a result *)
  | Some (Ok r) ->
      r
  | Some (Error (`Exn_trap (exn, bt))) ->
      Printexc.raise_with_backtrace exn bt
