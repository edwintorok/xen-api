let max_yield = 100_000

let rec worker yielded  =
  let previous = Atomic.fetch_and_add yielded 1 in
  Thread.yield ();
  if previous < max_yield then
    worker yielded

let () =
  let yielded = Atomic.make 0 in
  let threads = Array.init 1 (fun _ -> Thread.create worker yielded) in
  let start = Mtime_clock.counter () in
  worker yielded;
  let elapsed = Mtime_clock.count start in
  let actual_yields = Atomic.get yielded in  
  Array.iter Thread.join threads;
  let ns_per_yield = Int64.div (Mtime.Span.to_uint64_ns elapsed) (Int64.of_int actual_yields) in
  Format.printf "%Lu ns/sigyield; %a, %d@." ns_per_yield Mtime.Span.pp elapsed actual_yields
