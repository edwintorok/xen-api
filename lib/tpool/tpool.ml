(** A thread and domain-safe FIFO work-queue.

  This ensures low tail-latency for tasks that never suspend and run to completion.

  All operations are lock-free, thread-safe and domain-safe.
*)
module FIFO = Saturn_lockfree.Queue

(** [with_resource ~acquire ~release t f arg] is equivalent to the following, but doesn't allocate

  {[
    let r = acquire t in
    Fun.protect
      ~finally:(fun () -> release r)
      (fun () -> f r arg)
  ]}

  @param release must not raise any exceptions, and must preserve the backtrace.
    This is a stronger requirement than for [Fun.protect].
*)
let[@inline always] with_resource ~acquire ~release t f arg =
  let t = acquire t in
  match f t arg with r -> release t ; r | exception e -> release t ; raise e

module MakeWatchedCondition (C : sig
  type _ t

  val eval : _ t -> bool
  (** [eval t] evaluates the condition on [t].
    This must be thread-safe and domain-safe.
    *)
end) : sig
  type t

  val create : unit -> t

  val check_condition : t -> _ C.t -> unit

  val wait_condition : t -> _ C.t -> unit
end = struct
  type t = {mutex: Mutex.t; cond: Condition.t}

  let create () = {mutex= Mutex.create (); cond= Condition.create ()}

  let[@inline always] check_condition t arg =
    if C.eval arg then Condition.signal t.cond

  let acquire t = Mutex.lock t.mutex ; t

  let release t = Mutex.unlock t.mutex

  (** [with_mutex t f] is equivalent to the following, but doesn't allocate:

     {[
       Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex)
       (fun () -> Mutex.lock t.mutex; f t)
     ]}
    *)
  let[@inline never] with_mutex t f arg =
    with_resource ~acquire ~release t f arg

  (** [wait_condition_locked t arg] waits until [C.eval arg] is [true] *)
  let[@inline never] wait_condition_locked t arg =
    while not (C.eval arg) do
      Condition.wait t.cond t.mutex
      (* according to comments in [Domain_local_await] this may raise asynchronous exceptions,
         (e.g. from signal handlers),
         so we must run this within code that guarantees to always release the mutex,
         in our case {!val:with_mutex}
      *)
    done

  let[@inline always] wait_condition t arg =
    if C.eval arg then () else with_mutex t wait_condition_locked arg
end
[@@inline always]

module FIFONonEmpty = MakeWatchedCondition (struct
  type 'a t = 'a FIFO.t

  let eval t = not (FIFO.is_empty t)
end)

module ThrottledWorkQueue : sig
  type 'a t

  val create : unit -> _ t
  (** [create ()] createst a rate-limited FIFO work queue.
  
    @param soft is the number of tasks after which we start backing off  
  *)

  val idlers : _ t -> int
  (** [idlers t] is the number of workers that are idle, because this workqueue is empty.

    The number can be empty if we have more tasks queued than workers available to run them.
    *)

  val enter_idle : 'a t -> 'a t

  val leave_idle : _ t -> unit

  val is_empty : _ t -> bool

  val push : 'a t -> 'a -> unit
  (** [push t task] pushes the [task] into the queue [t].
    This implements a backoff strategy when more than [t.soft] tasks are queued in [t],
    but it doesn't block the current thread.
    *)

  val pop : 'a t -> 'a
  (** [pop t] returns the next task in FIFO order, or blocks until one becomes available. *)

  val wait : _ t -> unit
  (** [wait t] waits until the FIFO queue is empty *)
end = struct
  type 'a t = {fifo: 'a FIFO.t; nonempty: FIFONonEmpty.t; idlers: int Atomic.t}

  let[@inline always] idlers t = Atomic.get t.idlers

  let[@inline always] task_enqueued t = Atomic.decr t.idlers

  let[@inline always] task_deqeueud t = Atomic.incr t.idlers

  (* enter/leave is the opposite of enqueued/dequeud *)
  let[@inline always] enter_idle t = task_deqeueud t ; t

  let leave_idle = task_enqueued

  let[@inline always] is_empty t = FIFO.is_empty t.fifo

  let create () =
    { fifo= FIFO.create ()
    ; nonempty= FIFONonEmpty.create ()
    ; idlers= Atomic.make 0 }

  let[@inline always] push t task =
    (* This must always be done first to ensure FIFO ordering of tasks.
       Any rate-limiting or blocking sleeps can only be done after
    *)
    FIFO.push t.fifo task ;
    task_enqueued t ;
    (* Wake up one sleeping worker to handle the task.
       Due to spurious wakeups more than one could wake up,
       but this is better than Condition.broadcast which would always wake all of them.
       If all workers are busy then none would wake up, but they'll notice the non-empty queue
       when they loop around the next time.
    *)
    FIFONonEmpty.check_condition t.nonempty t.fifo ;
    if idlers t < 0 then
      (* To maintain FIFO guarantees we need to perform this *after* we've pushed the task.
         This rate-limits creation of new tasks when the queue is beyond its soft limit.
         Note that if all tasks spawn new tasks then there might be some thrashing on OCaml 4 between threads
      *)
      Domain.cpu_relax ()

  (* we cannot block here, or we'd risk deadlocking.
     The caller can block or spawn more threads if needed
     (the last worker is not allowed to sleep, it must always spawn a new thread)
  *)

  (** [wait_idle t] is the slowpath that waits for more work to become available. *)
  let wait_idle t () = FIFONonEmpty.wait_condition t.nonempty t.fifo

  (** [wait t] is the slowpath that waits for more work to become available,
    it increments and decrements [idlers t] when entering/leaving.
   *)
  let[@inline never] wait t =
    with_resource ~acquire:enter_idle ~release:leave_idle t wait_idle ()

  (** [pop t] returns a task from the FIFO queue in [t], or waits until one becomes available. *)
  let[@inline always] rec pop t =
    match FIFO.pop_opt t.fifo with
    | Some task ->
        task_deqeueud t ; task
    | None ->
        (* TODO: error prone, we need to wait on the actual thing, which is that this returns Some...
           otherwise we can easily get the condition inverted
        *)
        wait t ;
        (* we are not guaranteed to have a task here,
           another thread in another Domain may have already [pop]-ed it,
           so we need to loop and check.
        *)
        (pop [@tailcall]) t
end

module Worker = struct
  let raise_shutdown () = raise_notrace Thread.Exit

  let shutdown t = ThrottledWorkQueue.push t raise_shutdown

  let process_one t =
    (* will stop on Thread.Exit being raised *)
    let task = ThrottledWorkQueue.pop t in
    task ()
end

module Busy = MakeWatchedCondition (struct
  type 'a t = 'a ThrottledWorkQueue.t

  let eval t = ThrottledWorkQueue.idlers t < 0
end)

(** No more tasks, except the currently running one *)
module NumTasks1 = MakeWatchedCondition (struct
  type _ t = int Atomic.t

  let eval num_tasks = Atomic.get num_tasks = 1
end)

(** 0 tasks *)
module NoTasks = MakeWatchedCondition (struct
  type _ t = int Atomic.t

  let eval num_tasks = Atomic.get num_tasks = 0
end)

module Workers = struct
  type t =
    { q: (unit -> unit) ThrottledWorkQueue.t
    ; busy: Busy.t (* TODO: abstract thread spawning away too.. *)
    ; num_tasks: int Atomic.t
    ; shutdown: bool Atomic.t
    ; no_tasks: NoTasks.t
    ; num_tasks_one: NumTasks1.t
    ; on_exn: exn -> Printexc.raw_backtrace -> unit
    ; create_thread: (unit -> unit) -> Thread.t }

  let num_tasks t = Atomic.get t.num_tasks

  let[@inline always] wrap ~on_exn task =
    let f () =
      try task ()
      with e -> (
        let bt = Printexc.get_raw_backtrace () in
        try on_exn e bt with _ -> () )
    in
    f

  let[@inline never] raise_no_queue () = invalid_arg "Worker Queue is shut down"

  let[@inline always] run_internal t task =
    Atomic.incr t.num_tasks ;
    ThrottledWorkQueue.push t.q task

  let[@inline always] run t task =
    let task = wrap ~on_exn:t.on_exn task in
    if Atomic.get t.shutdown then raise_no_queue () ;
    run_internal t task ;
    Busy.check_condition t.busy t.q

  let worker t () =
    ThrottledWorkQueue.leave_idle t.q ;
    while not (Atomic.get t.shutdown) do
      (try Worker.process_one t.q with Thread.Exit -> ()) ;
      Atomic.decr t.num_tasks ;
      NumTasks1.check_condition t.num_tasks_one t.num_tasks ;
      NoTasks.check_condition t.no_tasks t.num_tasks
    done

  let shutdown t =
    run t
    @@ fun () ->
    (* 1 is this task, but we need to wait until there are no more *)
    NumTasks1.wait_condition t.num_tasks_one t.num_tasks ;
    Atomic.set t.shutdown true ;
    for _ = 1 to ThrottledWorkQueue.idlers t.q do
      run_internal t Worker.raise_shutdown
    done ;
    (* drain all shutdown tasks *)
    NumTasks1.wait_condition t.num_tasks_one t.num_tasks ;
    (* TODO: better way of waking watchdog *)
    ThrottledWorkQueue.leave_idle t.q ;
    (* shutdown current worker too *)
    Worker.raise_shutdown ()

  let shutdown_and_wait t =
    shutdown t ;
    NoTasks.wait_condition t.no_tasks t.num_tasks
  (* TODO: actually joinable, e.g. push exited threads to a queue?
     and wait for watchdog to exit too
  *)

  let make_worker t =
    let (_ : _ ThrottledWorkQueue.t) = ThrottledWorkQueue.enter_idle t.q in
    (* Thread.t will be GCed when done *)
    let (_ : Thread.t) = t.create_thread (worker t) in
    ()

  let rec watchdog t =
    (* TODO: should also decrement not just increment! *)
    Busy.wait_condition t.busy t.q ;
    if not (Atomic.get t.shutdown) then (
      (* need to change the above condition, so we don't keep spawning new threads *)
      make_worker t ;
      (* let other threads run, slow down the rate at which we spawn new threads *)
      Thread.yield () )
    else (watchdog [@tailcall]) t

  let create ~on_exn ~create_thread ~min_workers () =
    let q = ThrottledWorkQueue.create ()
    and busy = Busy.create ()
    and num_tasks = Atomic.make 0
    and shutdown = Atomic.make false
    and no_tasks = NoTasks.create ()
    and num_tasks_one = NumTasks1.create () in
    let t =
      { q
      ; busy
      ; num_tasks
      ; shutdown
      ; on_exn
      ; create_thread
      ; no_tasks
      ; num_tasks_one }
    in
    for _ = 1 to min_workers do
      make_worker t
    done ;
    let (_ : Thread.t) = create_thread (fun () -> watchdog t) in
    t
end

(*
module ThreadId = struct
  type t = Thread.t

  let compare a b = Int.compare (Thread.id a) (Thread.id b)
end

module ThreadSet = Set.Make (ThreadId)

exception WorkerExit (* not exposed *)

module Worker = struct
  (* not exposed *)
  exception ExitWorker

  let shutdown_one queue =
    WorkQueue.push queue @@ fun () -> raise_notrace ExitWorker i

  let rec run queue =
    match Bounded_queue.pop queue with
    | task ->
        let loop =
          try task () ; true with
          | ExitWorker ->
              false
          | e ->
              Thread.default_uncaught_exception_handler e ;
              true
        in
        if loop then (run [@tailcall]) queue
    | exception Bounded_queue.Closed ->
        ()
end

module Pool = struct
  type nonrec t =
    { exited: Thread.t Blocking_queue.t
    ; queue: WorkQueue.t
    ; num_tasks: int Atomic.t
    ; workers: int Atomic.t
    ; min_size: int
    ; runner: t option Atomic.t }

  let size t = WorkQueue.size t.queue

  let num_tasks t = Atomic.get t.num_tasks

  let create_worker t =
    let run t =
      TLS.get Runner.For_runner_implementors.k_cur_runner := Some t ;
      Worker.run t.queue
    in
    let (_ : Thread.t) =
      Thread.create run t (* TODO: from moonpool_dpool... *)
    in
    Atomic.incr t.workers

  let create ~min_size ~max_pending () =
    let t =
      { exited= Blocking_queue.create ()
      ; queue= WorkQueue.create ~max_size:max_pending ()
      ; num_tasks= Atomic.make 0
      ; workers= Atomic.make 0
      ; min_size }
    in
    let (_ : _ array) = Array.init min_size (fun _ -> create_worker t) in
    ()

  let shutdown ~wait t =
    WorkQueue.shutdown t.queue ;
    if wait then
      while Atomic.get t.num_tasks > 0 do
        Blocking_queue.pop t.exited |> Thread.join
      done ;
    Blocking_queue.close t.exited

  let wrap_task t f =
    let finally () =
      Blocking_queue.push t.exited (Thread.self ()) ;
      Atomic.decr t.num_tasks
    in
    Fun.protect ~finally f

  let run_async t ~ls task =
    let task = wrap_task t task in
    Bounded_queue.push t.queue task ;
    Atomic.incr t.num_tasks
end

let create () =
  let t = Pool.create () in
  let size () = Pool.size t
  and num_tasks () = Pool.num_tasks t
  and shutdown ~wait () = Pool.shutdown ~wait t in
  let runner =
    For_runner_implementors.create ~size ~num_tasks ~shutdown
      ~run_async:(Pool.run_async t) ()
  in
  Pool.set_runner runner

module TLS = Moonpool_private.Thread_local_storage_
include Runner
open Moonpool
*)
