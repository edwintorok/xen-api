(* based on Picos_threaded *)

open Picos
module Timer = Timer
module Nice = Nice

(* use Moonpool here... and a choice of runner,
 low latency: FIFO
 general: WS

 might need dynamic threads though
 *)
type global =
{ spawn: (t -> unit) Picos_mpsc_queue.t
; consumer: Mutex.t (** there can be only one consumer, which must use this lock *)
; condition: Condition.t
}

(** the current thread in the threadpool *)
and t =
{ current: Fiber.t Atomic.t
; mutex: Mutex.t
; condition: Condition.t
; global: global
}

let current t = Atomic.get t.current

let yield t =
  let fiber = current t in
  Fiber.check fiber;
  Thread.yield ()

let cancel_after t computation ~seconds bt =
  let fiber = current t in
  Fiber.check fiber;
  Picos_select.cancel_after computation ~seconds bt

let resume trigger t _ =
  let fiber = current t in
  let (_: bool) = Fiber.unsuspend fiber trigger in
  Condition.broadcast t.condition

let[@alert "-handler"] await t trigger =
  let fiber = current t in
  if Fiber.try_suspend fiber trigger t t resume then
    Mutex.lock t.mutex;
    while not (Trigger.is_signaled trigger) do
      Condition.wait t.condition t.mutex
    done;
    Mutex.unlock t.mutex;
  Fiber.canceled fiber

let spawn : type a. _ -> forbid:bool -> a Computation.t -> _ =
 fun t ~forbid computation mains ->
  let fiber = current t in
  Fiber.check fiber;
  let packed = Computation.Packed computation in
  let run main t =
    let fiber = Fiber.create_packed ~forbid packed in
    Atomic.set t.current fiber;
    main ()
  in
  mains
  |> List.iter @@ fun main -> Picos_mpsc_queue.push t.global.spawn @@ run main
(*     Thread.create
       (fun () ->
         (* We need to (recursively) install the handler on each new thread
            that we create. *)
         Handler.using handler (create_packed ~forbid packed) main)
       ()
     |> ignore*)


let handler =
Handler.
{ current
; spawn
; yield
; await
; cancel_after
}

let create_context ~forbid global =
  let current = Atomic.make (Fiber.create ~forbid (Computation.create ()))
  and mutex = Mutex.create ()
  and condition = Condition.create () in
  { current; mutex; condition; global  }

let rec scheduler_next_locked global =
  match Picos_mpsc_queue.pop_exn global.spawn with
  | work ->
    Mutex.unlock global.consumer;
    work
  | exception Picos_mpsc_queue.Empty ->
    Condition.wait global.condition global.consumer;
    scheduler_next_locked global

let scheduler_next global =
  Mutex.lock global.consumer;
  scheduler_next_locked global
  

let run_thread t =
  (* TODO: check pool cancelation *)
  while true do
    let work = scheduler_next t.global in
    try work t
    with _ -> (* TODO *) ()
  done

let run ~forbid ~initial f =
  Picos_select.check_configured ();
  let global = { spawn = Picos_mpsc_queue.create (); consumer = Mutex.create (); condition = Condition.create () } in
  let threads = Array.init initial (
    Thread.create @@ fun _ ->
      let t = create_context ~forbid global in
      Handler.using handler t (fun () -> run_thread t)
  ) in
  let t = create_context ~forbid global in
  Handler.using handler t f;
  Array.iter Thread.join threads

