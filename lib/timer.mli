type t

(** [with_cpu_timer ~is_thread ~interval f] creates a POSIX timer with [timer_create],
  and sets it to deliver [SIGVTALRM] whenever the process or thread has consumed [interval] seconds of CPU time.
  The signal will be redelivered periodically every time the process or thread consumes [interval] more seconds of CPU time.
  The timer is disarmed and destroyed when [f] finishes.

  This can be used to limit the amount of time a single thread can hold the OCaml master lock.

  @param is_thread whether to measure time per thread or per process
  @param interval in seconds when to deliver [SIGVTALRM]
  @param f the function to call with this timer armed
 *)
val with_cpu_timer: is_thread:bool -> interval:float -> (t -> 'a) -> 'a

(** [cpu_timer_settime t interval] changes the [SIGVTALRM] delivery interval to [interval].
  A [0.] value disarms the timer.
 *)
val cpu_timer_settime: t -> float -> unit

(** [cpu_timer_gettime t] retrieves the current value of the timer [t] in seconds. *)
val cpu_timer_gettime: t -> float

val default_interval: float Atomic.t

val thread_create: ('a -> unit) -> 'a -> Thread.t
(** [thread_create f arg] is a wrapper for [Thread.create f arg] that sets up an interval timer with {!val:with_cpu_timer},
  with a default interval.
 *)

val set_sigio: Unix.file_descr -> unit
