module StartStop = struct
  type t = 
  { mutex: Mutex.t
  ; cond: Condition.t
  ; start: bool Atomic.t  
  ; stop: bool Atomic.t
  }

  let wait_start t =
    Mutex.lock t.mutex;
    let finally () = Mutex.unlock t.mutex in
    Fun.protect ~finally @@ fun () ->
    while not (Atomic.get t.start) do
      Condition.wait t.cond t.mutex
    done

  let should_stop t = Atomic.get t.stop

  let make () = {
      mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; start = Atomic.make false
    ; stop = Atomic.make false
  }

  let start t =
    Mutex.lock t.mutex;
    Atomic.set t.start true;
    Mutex.unlock t.mutex;
    Condition.broadcast t.cond

  let stop t = Atomic.set t.stop true
end

let worker ready f _ =
  StartStop.wait_start ready;
  while not (StartStop.should_stop ready) do
    f ()
  done

let allocate f n =
  let ready = StartStop.make () in
  ready, Array.init n @@ Thread.create (worker ready f)

let free (ready, a) =
  StartStop.start ready;
  StartStop.stop ready;
  Array.iter Thread.join a
