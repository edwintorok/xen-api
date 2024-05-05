let run_id () =
  Printf.printf "Thread id: %d\n%!" (Thread.id @@ Thread.self ())

let () =
  let pool =
    Tpool.Workers.create
      ~on_exn:(fun _ _ -> ())
      ~create_thread:(fun f -> Thread.create f ())
      ~min_workers:1 ()
  in
  Thread.delay 1.;
  Tpool.Workers.run pool run_id;
  Tpool.Workers.run pool run_id;
  Tpool.Workers.run pool run_id;
  Thread.delay 1.;
  Tpool.Workers.run pool run_id;
  Tpool.Workers.run pool run_id;
  Tpool.Workers.run pool run_id;
  Tpool.Workers.shutdown_and_wait pool
