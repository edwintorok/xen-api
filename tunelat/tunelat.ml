open Simple_workloads

let stop = Atomic.make false
let worker () =
  while not (Atomic.get stop) do
    Operations.Yield.perform ()
  done

let max_overhead_percentage = 0.5

let () =
  let t = Thread.create worker () in
  let yield_lo, yield_overhead, yield_hi = Simple_measure.measure Operations.Yield.perform Operations.Yield.count in
  Printf.printf "Yield overhead: %.7fs,%.7fs,%.7fs\n" yield_lo yield_overhead yield_hi;
  Atomic.set stop true;
  Thread.join t;

  let timeslice_min = yield_overhead /. (max_overhead_percentage /. 100.) in
  Printf.printf "timeslice: %.6fs\n" timeslice_min

