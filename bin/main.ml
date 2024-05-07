open Picos
let loop _ () =
  let t = ref [] in
  for _ = 1 to 100000 do
    t := Sys.opaque_identity (List.init 10000 (fun _ -> String.make 1 'x'))
  done

let busy () =
  let computation = Computation.create () in
  while true; do
    Fiber.spawn ~forbid:false computation (List.init 100 loop);
    (* TODO: scheduler needs queue length limit *)
(*    prerr_endline "yield";*)
    Fiber.sleep ~seconds:0.01
  done

let low_latency () =
  let computation = Computation.create () in
  let measurements = Queue.create () in
  let m = Mutex.create () in
  let rec measure_latency t0 () =
    let t1 = Unix.gettimeofday () in (* TODO: mtime *)
    (* TODO: fiber scheduler: account for time spent *)
    Mutex.lock m;
    Queue.push (t1 -. t0) measurements;
    let avg =
      if Queue.length measurements > 100 then begin
        let sum = Queue.fold (+.) 0. measurements in
        let maxi = Queue.fold Float.max 0. measurements in
        let n = Queue.length measurements in
        Queue.clear measurements;
        Some (sum /. float n, maxi)
      end
      else None
    in
    Mutex.unlock m;
    begin match avg with
    | None -> ()
    | Some (avg, maxi) ->
      Printf.printf "%.6fs, %.6fs\n%!" avg maxi;
      Fiber.sleep ~seconds:1.
    end;
    let t2 = Unix.gettimeofday () in (* TODO: mtime *)
    Fiber.spawn ~forbid:false computation [measure_latency t2]
  in
  let t0 = Unix.gettimeofday () in
  Fiber.spawn ~forbid:false computation
  (List.init 16 @@ fun _ -> fun () ->
   (* let (_:int) =
     try Picos_prio.nice (-19)
     with e ->
       Printexc.to_string e |> prerr_endline;
       0
     in*)
    measure_latency t0 ()
  )

(*let wake (_:int) = Thread.yield ()*)

let run_thread (f, arg) =
  Picos_prio.set_thread_timer 5_000_000L;
  f arg

let thread_create f arg =
  Thread.create run_thread (f, arg)

let () = 
  (*Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle wake);*)
  (*let (_:Unix.interval_timer_status) = Unix.setitimer Unix.ITIMER_VIRTUAL Unix.{it_interval = 0.0001; it_value = 0.0001} in*)
  Picos_select.configure ();
  (* TODO: should grow dynamically *)
  let t2 = thread_create (Picos_prio.run ~forbid:false ~initial:16) low_latency in
  let t1 = thread_create (Picos_prio.run ~forbid:false ~initial:1000) busy in
  Thread.join t1;
  Thread.join t2
