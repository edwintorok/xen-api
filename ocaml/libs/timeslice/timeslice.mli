val set : ?real:bool -> float -> unit
(** [set ?real interval] calls [Thread.yield ()] every [interval] seconds using a POSIX timer.

  @param ?real whether to use {!val:Unix.ITIMER_REAL} or {!val:Unix.ITIMER_VIRTUAL}. The default is to use virtual timers.

  An [ITIMER_REAL] timer will deliver a signal every [interval] seconds, even if the process is otherwise idle.
  This is similar to how OCaml's tick thread currently works (except it uses a separate thread that calls [sleep] instead).
  It also supports very short intervals, which is useful for testing.

  An [ITIMER_VIRTUAL] timer will deliver a signal every [interval] seconds, but only if the thread has actually used the CPU for that period of time.
  This is better, because idle threads/processes don't cause extra overhead with needlessly switching between idle threads.
  Very short intervals get rounded up by the kernel, usually to [1/HZ] (currently ~4ms in our Dom0, but could also be 1ms in the future).

  The implementation of [Thread.yield] guarantees since OCaml 4.09 that we'll switch to a different OCaml thread,
  if one exists that is not blocked (i.e. it doesn't rely on [sched_yield] which may run the same thread again,
  but uses pthread mutexes and condition variables to ensure the current thread isn't immediately runnable).

  The setting is global for the entire process, and takes control over the per-process interval timer,
  i.e. assumes that nothing else is using it in the application.
  It also takes control of [ALRM] or [VTALRM] signal handlers as appropriate.

  (If this turns out to be a problem in the future we could use POSIX [timer_create] API instead,
   which allows arbitrary per-thread timers to be created)

  @raises Invalid_argument if the signal handler or timer is already set
*)

val clear : ?real:bool -> unit -> unit
(** [clear ?real ()] undoes the changes made by [set].
  This is useful for testing multiple timeslices in the same program. *)
