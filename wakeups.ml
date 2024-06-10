(* depending on C-state target residency,
   a sufficient number of OCaml processes would prevent entering deep C states,
   e.g. if their wakeups are offset in such a way as to prevent entering the sleep states.

   This needs > logical cores * 50000 / <deepest-C-state-latency>

   Skylake 16 * 50000/ 5890

   Powertop perhaps?

   
  *)

let () =
  Gc.print_stat stderr;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1));
  at_exit (fun () -> Gc.print_stat stderr);
  let t = Array.init 1000 (Thread.create @@ fun _ ->
   Unix.sleepf 3600.) in
 (* Unix.sigsuspend []; 

   signals aren't being processed if we're sleeping on Thread.join,
   so we must block them
 *)
  let prev = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]  in
  Array.iter Thread.join t;
  let (_:_ list) = Thread.sigmask Unix.SIG_SETMASK prev in ()
