open Bechamel

let api_pool = Threadpool.create ~name:"api" ignore ignore 16

let concurrently n m f =
  let sem = Semaphore.Counting.make m in
  let finally () = Semaphore.Counting.release sem in
  let make_thread i =
    Semaphore.Counting.acquire sem;
    Fun.protect ~finally (fun () -> f i)
  in
  (* simulate XAPI spawning a thread for each API call, this will be part of the measured overhead,
     because it is how the API calls are handled, although improvements here are possible by using a threadpool,
     which can also be benchmarked *)
  (*let threads = Array.init n (fun _ -> Threadpool.run_in_pool' api_pool (make_thread)) in
  Array.iter (fun f -> f()) threads*)
  let threads = Array.init n (Thread.create make_thread) in
  Array.iter Thread.join threads

let bench_tracing = true

let allocate n = Threadpool.create ~name:"authw" Pam.authenticate_start Pam.authenticate_stop n
let free = ignore
  (* Threadpool.shutdown*)

let test name execute =
  Test.make_indexed_with_resource ~name ~args:[8]
  ~allocate
  ~free
  Bechamel.Test.uniq
  (fun i -> Staged.stage @@ fun pool ->
    concurrently 32 i (execute pool)
  )

let auth_pam pam = Pam.authorize pam "pamtest-edvint" "pamtest-edvint"

let auth_user pool _ =
  Threadpool.run_in_pool pool auth_pam

let benchmarks =
  Test.make_grouped ~name:"Auth"
    ([test "auth" auth_user
     ]
    )

let () =
  Gc.compact () ;
  Bechamel_simple_cli.cli benchmarks
