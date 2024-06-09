let alloc n =
  (* essential for each element to store the value of 1, so we can count how many elements we have accessed *)
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

let rec read_stride ~data ~stride ~mask ~remaining ~pos =
  if remaining > 0 then
    let remaining = remaining - Char.code (String.unsafe_get data pos)
    and pos = (pos + stride) land mask in
    read_stride ~data ~stride ~mask ~remaining ~pos
  else 
    Sys.opaque_identity remaining

(* should be larger than all caches *)
let max_data_size_log2 = 28

(* should be just below real L1 size *)
let min_cache_size_log2 = 14

(* should be below real cache line size *)
let min_stride_log2 = 4

let operations_log2 = max_data_size_log2 + 1
  
let measure_stride_op_time ~data ~stride ~size_log2 =
  assert (operations_log2 > max_data_size_log2);
  let remaining = 1 lsl operations_log2
  and mask = (1 lsl size_log2) - 1 in
  assert (mask < String.length data);
  let t0 = cpu_time () in
  let r =  read_stride ~data ~stride ~mask ~remaining ~pos:0 in
  let t1 = cpu_time () in
  (* use result to prevent the compiler optimizing it away *)
  let _ = Sys.opaque_identity r in
  float (t1 - t0) /. float remaining

let print_suffix n =
  if n < 1 lsl 10 then
    Printf.printf "%d" n
  else if n < 1 lsl 20 then
    Printf.printf "%dKiB" (n lsr 10)
  else Printf.printf "%dMiB" (n lsr 20)



let () =
  let data = alloc (1 lsl max_data_size_log2) in
  Printf.printf "stride/cachesize";
  for size_log2 = max_data_size_log2 downto min_cache_size_log2 do
    print_char ' ';
    print_suffix (1 lsl size_log2)
  done;
  Printf.printf "\n%!";
  for stride_log2 = min_stride_log2 to max_data_size_log2-1 do
    let stride = 1 lsl stride_log2 in
    Printf.printf "%d" stride;
    (* we can only skip columns at the end, so have to invert iteration order here *)
    for size_log2 = max_data_size_log2 downto min_cache_size_log2 do
      if stride_log2 < size_log2 then
        let time = measure_stride_op_time ~data ~stride ~size_log2 in
        Printf.printf " %.1f%!" time
    done;
    Printf.printf "\n"
  done
  
