(** [large_sleep ()] sleeps for a very long time, releases the runtime lock,
  and handles signals. *)
let large_sleep () =
  Unix.sleepf 86400.

(** [thread_idle_wait ()] is a pair [run, shutdown] that creates a thread that waits until [shutdown] is called. The thread is otherwise idle, except for spurious wakeups. *)
let thread_idle_wait () =
  let mutex = Mutex.create ()
  and cond = Condition.create ()
  and stop = Atomic.make false (* doesn't necesarily have to be atomic *)
  in
  let finally () = Mutex.unlock mutex in
  let worker () =
    Mutex.lock mutex;
    Fun.protect ~finally @@ fun () ->
    while not (Atomic.get stop) do
      Condition.wait cond mutex
    done
  and shutdown () =
    Atomic.set stop true;
    Condition.broadcast cond
  in
  Thread.create worker, shutdown 

(** [uninterruptible_sleep ()] returns a pair [run, shutdown], where [run] will wait for [shutdown] to be called, but it won't consume CPU, and won't be interruptible, although it does release the runtime lock.
  This is perhaps a bug currently that [Thread.join] and similar functions can block indefinitely
  and not generate EINTR internally.
 *)
let uninterruptible_sleep () =
  let thread, shutdown = thread_idle_wait () in
  let run () =
    let t = thread () in
    Thread.join t
  in
  run, shutdown
