open Bechamel_perf.Instance

let instructions_per_cycle = Derived.div "IPC" cycles instructions

let cache_miss_ratio =
  Derived.percent_div "cache-miss-ratio" cache_misses cache_references

let branch_miss_ratio =
  Derived.percent_div "branch-miss-ratio" branch_misses branch_instructions

let instances =
  [ instructions_per_cycle
  ; cache_miss_ratio
  ; branch_miss_ratio
  ; page_faults
  ; context_switches
  ; cpu_migrations
  ; alignment_faults
  ; emulation_faults
  ; task_clock ]
