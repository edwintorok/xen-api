external parallel_c_work : int -> unit = "caml_bench_concurrent_parallel_c_work"

let semaphore () = Semaphore.Binary.make false

let wait = Semaphore.Binary.acquire

let signal = Semaphore.Binary.release

(* this uses the 'barrier binary array' implementation technique, see benchmarks in bench_concurrent *)
module Worker = struct
  type 'a t = {
      start: Semaphore.Binary.t
    ; stopped: Semaphore.Binary.t
    ; quit: bool Atomic.t
  }

  let worker ~allocate ~free ~run t =
    let resource = allocate () in
    let finally () = free resource in
    let rec worker_loop () =
      wait t.start ;
      if not (Atomic.get t.quit) then (
        (* TODO: exceptions.. *)
        let () = Sys.opaque_identity @@ Bechamel.Staged.unstage run resource in
        signal t.stopped ; worker_loop ()
      )
    in
    Fun.protect ~finally worker_loop

  let make ~allocate ~free ~run _ =
    let start = semaphore () and stopped = semaphore () in
    let t = {start; stopped; quit= Atomic.make false} in
    (t, Thread.create (worker ~allocate ~free ~run) t)

  let signal_start (t, _) = signal t.start

  let wait_stop (t, _) = wait t.stopped

  let set_quit (t, _) =
    let ok = Atomic.compare_and_set t.quit false true in
    assert ok (* detect double free *)

  let join_thread (_, thread) = Thread.join thread

  let barrier_wait all =
    (* must first start all, do not combine this with waiting *)
    Array.iter signal_start all ;

    (* wait until all are finished, benchmark code is running in the thread now *)
    Array.iter wait_stop all

  let shutdown all =
    Array.iter set_quit all ;
    Array.iter signal_start all ;
    Array.iter join_thread all
end

let test_concurrently ?(threads = [1; 4; 8; 16]) ~allocate ~free ~name run =
  let open Bechamel in
  let allocate n = Array.init n @@ Worker.make ~allocate ~free ~run in
  let free all = Worker.shutdown all in
  Test.make_indexed_with_resource ~name ~args:threads Test.multiple ~allocate
    ~free (fun _ -> Staged.stage Worker.barrier_wait
  )