open Bechamel

module Rdtsc = struct
  type witness = unit
  let label () = "TSC"
  let make () = ()
  let unit () = "cycles"
  let load = ignore
  let unload = ignore

(* an [lfence] is a serializing instruction lately on Intel and AMD,
see https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=be261ffce6f13229dad50f59c5e491f933d3167f.
  This can be used as a better alternative to [cpuid; rdtsc], because cpuid might be intercepted by a hypervisor, and introduce unpredictable overhead.  
  *)
  let serialize = Ocaml_intrinsics.Fences.load_fence
  
  let get () =
    serialize ();
    Ocaml_intrinsics.Perfmon.rdtsc () |> Int64.to_float
end

let rdtsc =
  let measure = Measure.register (module Rdtsc) in
  Measure.instance (module Rdtsc) measure
  
let instances = [ rdtsc ]
