let () =
  let work, _stop = Workloads.Cpu.cache_misses () in
  work ()
