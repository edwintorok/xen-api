let () =
  let nthreads = ref 8 in
  let user = ref "" in
  let password = ref "" in
  let perthread = ref 4  in
  (* TODO: use bechamel to measure speed! *)
  Arg.parse [
    "--threads", Arg.Set_int nthreads, "Number of threads to use"
  ; "--username", Arg.Set_string user, "Username for test"
  ; "--password", Arg.Set_string password, "Password for test"
  ; "--perthread", Arg.Set_int perthread, "Authentication attempts per thread"
  ] (fun _ -> raise (Arg.Bad "Unexpected argument")) "Usage";

  let start_semaphore = Semaphore.Counting.make !nthreads in
  let finish_semaphore = Semaphore.Counting.make 0 in

  let threads = Array.init !nthreads @@ Thread.create @@ fun _ ->
    let handle = Pam.authenticate_start () in
    let finally () = Pam.authenticate_stop handle in
    Fun.protect ~finally @@ fun () ->

    Semaphore.Counting.acquire start_semaphore;

    for _ = 1 to !perthread do
      Pam.authorize handle !user !password;
    done;
    
    Semaphore.Counting.release finish_semaphore
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

  let dt = Mtime_clock.counter () in
  start ();
  wait ();
  let speed = float (!nthreads * !perthread) /. (Mtime_clock.count dt |> Mtime.Span.to_s) in
  Printf.printf "Auth/s: %.3f\n" speed;
  
  Array.iter Thread.join threads