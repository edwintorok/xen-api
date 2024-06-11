open Bechamel_perf.Instance

let instructions_per_cycle = Measurable.div ~unit:"insn/cycle" ~scale:1. "IPC" cycles instructions

let cache_miss_ratio =
  Measurable.percentage "cache-miss-ratio" cache_misses cache_references

let branch_miss_ratio =
  Measurable.percentage "branch-miss-ratio" branch_misses branch_instructions

let frequency =
  (* cycles/ns *)
  Measurable.div ~unit:"GHz" ~scale:1. "frequency" cycles Bechamel.Toolkit.Instance.monotonic_clock

let measurables =
  [ instructions_per_cycle
  ; cache_miss_ratio
  ; branch_miss_ratio
  ; frequency
  ; Measurable.cumulative page_faults
  ; Measurable.cumulative context_switches
  ; Measurable.cumulative cpu_migrations
  ; Measurable.cumulative alignment_faults
  ; Measurable.cumulative emulation_faults
  ; Measurable.cumulative task_clock ]
