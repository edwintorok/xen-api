let alloc n =
  let a = String.make n '\x01' in
  (* move it to major heap *)
  Gc.full_major ();
  (* wait until the store has completed *)
  Ocaml_intrinsics.Fences.memory_fence ();
  (* make the compiler forget any knowledge about the contents of [a],
     to prevent optimizing away the benchmark loop
   *)
  Sys.opaque_identity a

(* an [lfence] is a serializing instruction lately on Intel and AMD,
see https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=be261ffce6f13229dad50f59c5e491f933d3167f.
  This can be used as a better alternative to [cpuid; rdtsc], because cpuid might be intercepted by a hypervisor, and introduce unpredictable overhead.
  
  *)
let serialize = Ocaml_intrinsics.Fences.load_fence

let[@inline always] cpu_time () =
  serialize ();
  Ocaml_intrinsics.Perfmon.rdtsc () |> Int64.to_int

let rec read_kth array ~k ~sum ~pos =
  if pos >= 0 then
    let sum = sum + Char.code (String.unsafe_get array pos) in
    read_kth array ~k ~sum ~pos:(pos-k)
  else
    sum

let maxk_log2 = 8
  
let measure_kth_read array k =
  let len = String.length array in
  (* perform same number of read operations, regardless of [k] *)
  let n = k * (len lsr maxk_log2) in
  let pos = n - k in
  assert (pos < len);
  let t0 = cpu_time () in
  let r =  read_kth array ~k ~sum:0 ~pos in
  let t1 = cpu_time () in
  (* use result to prevent the compiler optimizing it away *)
  let _ = Sys.opaque_identity r in
  t1 - t0

let read_all a =
  (* search for something that is not present *)
  Sys.opaque_identity (String.contains a 'x')

let () =
  (* TODO: use cpuid module, or parse [getconf -a] output *)
  let l1cache_size = 32 * 1024
  and l2cache_size = 1024 * 1024
  and l3cache_size = 32 * 1024 * 1024
  in
  let all_cache_size = l1cache_size + l2cache_size + l3cache_size in
  (* should be larger than all caches *)
  let n = all_cache_size * 4 in
  let a = alloc n
  and flush = alloc n
  in
  for k = 1 to 1 lsl maxk_log2 do
    (* we could do this in log2 steps, but for plotting it might be useful to see all *)
    let present = read_all flush in
    Printf.printf "%d %d\n%!" k (measure_kth_read a k);
    let (present:bool) = Sys.opaque_identity present in
    assert (not present);
    ()
  done
  (* this says my cacheline size is 128 bytes, not 64 as CPUID says... *)
