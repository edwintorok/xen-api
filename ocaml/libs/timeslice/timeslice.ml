let handler (_ : int) = Thread.yield ()

let sigkind real =
  if real then
    (* Not really needed on OCaml 4, there is internal special-cased handling for this signal as yield
       (without actually having to raise the signal, it just sets a global flag that is checked at certain points).
       But OCaml 5 uses a different interrupt mechanism, and it is needed there *)
    (Sys.sigalrm, Unix.ITIMER_REAL)
  else
    (Sys.sigvtalrm, Unix.ITIMER_VIRTUAL)

let set ?(real = false) interval =
  let signal, kind = sigkind real in

  (* ensure we don't override something else in the application that
     might be using this signal *)
  Sys.set_signal signal (Sys.Signal_handle handler);

  (* ensure that nothing else uses the timer yet *)
  let prev =
    Unix.setitimer kind Unix.{it_value= interval; it_interval= interval}
  in
  if prev.it_value <> 0. then
    invalid_arg
      (Printf.sprintf "interval timer already set: %fs,%fs" prev.it_value
         prev.it_interval
      )

let clear ?(real = false) () =
  let signal, kind = sigkind real in
  let (_ : Unix.interval_timer_status) =
    Unix.setitimer kind Unix.{it_value= 0.; it_interval= 0.}
  in
  (* be safe, and assume there might be some spurious deliveries *)
  Sys.set_signal signal Sys.Signal_ignore
