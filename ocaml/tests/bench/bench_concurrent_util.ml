external bench_fixed_work : int -> int = "caml_bench_fixed_work"

(*
  We could calibrate but that would still change the value slightly from invocation to invocation on the same machine,
  e.g. when running inside a VM and getting timeslices unpredictably, or if turbo is used, etc.

  let rec calibrate n =
  let t0 = Monotonic_clock.now () in
  let (_:int) = Sys.opaque_identity (bench_fixed_work n) in
  let t1 = Monotonic_clock.now () in
  let dt = Int64.sub t1 t0 in
  Format.eprintf "Calibrating: %d -> %Luns@." n dt;
  if Int64.compare dt 100_000_000L < 0 then
    calibrate (n * 2)
  else Int64.(div (mul (of_int n) 1_000_000L)  dt |> to_int)

let parallel_c_work =
  let n=  calibrate 10_000_000 in
  Format.eprintf "Calibrated: %d ~ 1ms@." n ;
  fun ms ->
  let (_:int) = Sys.opaque_identity (bench_fixed_work (ms * n)) in ()*)

(*
   So this will change with different compiler versions or CPU architectures,
   but will be fixed on a given system.
*)
let parallel_c_work () =
  let (_ : int) = Sys.opaque_identity @@ bench_fixed_work @@ 4_000_000 in
  ()

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
