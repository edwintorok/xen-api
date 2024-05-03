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
  Fiber.sleep ~seconds:1.
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
      if Queue.length measurements > 10 then begin
        let sum = Queue.fold (+.) 0. measurements in
        let n = Queue.length measurements in
        Queue.clear measurements;
        Some (sum /. float n)
      end
      else None
    in
    Mutex.unlock m;
    begin match avg with
    | None -> ()
    | Some avg ->
      Printf.printf "%.6fs\n%!" avg;
      Fiber.sleep ~seconds:1.
    end;
    let t2 = Unix.gettimeofday () in (* TODO: mtime *)
    Fiber.spawn ~forbid:false computation [measure_latency t2]
  in
  let t0 = Unix.gettimeofday () in
  Fiber.spawn ~forbid:false computation
  (List.init 16 @@ fun _ -> measure_latency t0)

let () = 
  Picos_select.configure ();
  (* TODO: should grow dynamically *)
  let t1 = Thread.create (Picos_prio.run ~forbid:false ~initial:10) busy in
  let t2 = Thread.create (Picos_prio.run ~forbid:false ~initial:16) low_latency in
  Thread.join t1;
  Thread.join t2
