type t

external cpu_timer_create: bool -> t = "ml_cpu_timer_create"
external cpu_timer_destroy: t -> unit = "ml_cpu_timer_destroy"

external cpu_timer_settime : t -> float -> unit = "ml_cpu_timer_settime"
external cpu_timer_gettime : t -> float = "ml_cpu_timer_gettime"

external set_sigio : Unix.file_descr -> unit = "ml_set_sigio"

let with_cpu_timer ~is_thread ~interval f =
  let t = cpu_timer_create is_thread in
  let finally () = cpu_timer_destroy t in
  Fun.protect ~finally @@ fun () ->
  cpu_timer_settime t interval;
  f t

let default_interval = Atomic.make 0.0075

let thread_create f arg =
  Thread.create (with_cpu_timer ~is_thread:true ~interval:(Atomic.get default_interval)) (fun (_: t) : unit -> f arg)
