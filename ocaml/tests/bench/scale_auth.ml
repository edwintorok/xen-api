let () =
  let nthreads = ref 8 in
  let user = ref "" in
  let password = ref "" in
  let perthread = ref 4 in
  let loops = ref 10 in
  let warmup = ref true in
  let warmup_close = ref false in
  (* TODO: use bechamel to measure speed! *)
  Arg.parse
    [
      ("--threads", Arg.Set_int nthreads, "Number of threads to use")
    ; ("--username", Arg.Set_string user, "Username for test")
    ; ("--password", Arg.Set_string password, "Password for test")
    ; ("--loops", Arg.Set_int loops, "loops")
    ; ( "--perthread"
      , Arg.Set_int perthread
      , "Authentication attempts per thread"
      )
    ; ("--nowarmup", Arg.Clear warmup, "nowarmup")
    ; ("--warmup-close", Arg.Clear warmup, "close pam handle after warmup")
    ]
    (fun _ -> raise (Arg.Bad "Unexpected argument"))
    "Usage" ;

  let start_semaphore = Semaphore.Counting.make 0 in
  let finish_semaphore = Semaphore.Counting.make 0 in
  let stop = Atomic.make false in

  let threads =
    Array.init !nthreads
    @@ Thread.create
    @@ fun _ ->
    let handle = Pam.authenticate_start () in
    let finally () = Pam.authenticate_stop handle in
    Fun.protect ~finally @@ fun () ->
    while not (Atomic.get stop) do
      Semaphore.Counting.acquire start_semaphore ;

      for _ = 1 to !perthread do
        Pam.authorize handle !user !password
      done ;

      Semaphore.Counting.release finish_semaphore
    done
  in

  let start () =
    for _ = 1 to !nthreads do
      Semaphore.Counting.release start_semaphore
    done
  in

  let wait () =
    for _ = 1 to !nthreads do
      Semaphore.Counting.acquire finish_semaphore
    done
  in

  let handle =
    if !warmup then
      let handle = Pam.authenticate_start () in
      (* warm up PAM, there is a sleep(1) inside otherwise on init race condition *)
      let () = Pam.authorize handle !user !password in
      if !warmup_close then begin Pam.authenticate_stop handle; None end
      else
      Some handle
    else
      None
  in

  let dt = Mtime_clock.counter () in
  for _ = 1 to !loops do
    start () ; wait ()
  done ;
  let speed =
    float (!nthreads * !perthread * !loops)
    /. (Mtime_clock.count dt |> Mtime.Span.to_s)
  in
  Option.iter Pam.authenticate_stop handle ;
  Atomic.set stop true ;
  start () ;
  (* don't do this above because that would unload the library *)
  Printf.printf "Auth/s: %.3f\n" speed ;

  Array.iter Thread.join threads
