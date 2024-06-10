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

  let domain_work () =
    let t = Array.init 2 (Thread.create @@ fun _ ->
      Unix.sleepf 3600.
    ) in
    let prev = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]  in
    Array.iter Thread.join t;
    let (_:_ list) = Thread.sigmask Unix.SIG_SETMASK prev in ()
  in

  let count = Domain.recommended_domain_count () in
  let domains = Array.init count
    (fun _ ->
      let d = Domain.spawn domain_work in
      Unix.sleepf (0.05 /. float count);
      d
    ) in
    let prev = Thread.sigmask Unix.SIG_BLOCK [Sys.sigint]  in
  Array.iter Domain.join domains;
    let (_:_ list) = Thread.sigmask Unix.SIG_SETMASK prev in ()
  
