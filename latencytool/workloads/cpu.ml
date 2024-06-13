(** Simple, CPU-intensive workloads *)

(** [noalloc_infinite_loop ()] an infinite loop, but it will still contain a poll point,
    where OCaml can react to changing minor heap limit,
    to invoke signal handlers.
 *)
let noalloc_infinite_loop () =
  while true do
    ()
  done

(** [loop_with_shutdown init f] returns a pair [loop, shutdown], where calling [loop ()] will run until
  [shutdown ()], e.g. in another thread. [loop ()] will call [f arg] repeatedly, where [arg = init ()]
 *)
let loop_with_shutdown init f =
  prerr_endline "START" ;
  flush stderr ;
  let arg = init () in
  prerr_endline "INIT" ;
  flush stderr ;
  let stop = Atomic.make false in
  let loop () =
    while not (Atomic.get stop) do
      f (Sys.opaque_identity arg)
    done ;
    (* prevent [arg] from being completely optimized out if its effects cannot be observed. *)
    let _ = Sys.opaque_identity arg in
    ()
  and shutdown () = Atomic.set stop true in
  (loop, shutdown)

let alloc_cycle q =
  let str = String.make 200 (Sys.opaque_identity 'x') in
  Queue.push str q ;
  if Queue.length q > 100 then Queue.clear q

(** [alloc_cycle ()] allocates and keeps allocated a small amount of data that usually fits in the minor heap,
  and then releases the reference so that it can be garbage collected.
 *)
let alloc_loop () = loop_with_shutdown Queue.create alloc_cycle

let copy n =
  let str = String.make n 'x' in
  let copy_n dest =
    Bytes.blit_string (Sys.opaque_identity str) 0 dest 0 (String.length str)
  and alloc () = Bytes.create n in
  loop_with_shutdown alloc copy_n

(** [copy_small ()] copies a small, 16KiB string repeatedly. The copy itself is not interruptible.
 *)
let copy_small () = copy (1 lsl 14)

(** [copy_large ()] copies a large, 16MiB string repeatedly. The copy itself is not interruptible,
  with the current implementation in the OCaml runtime.
 *)
let copy_large () = copy (1 lsl 24)

module IntMap = Map.Make (Int)

let char_add acc c = acc + Char.code c

let () = Random.self_init ()

let cache_misses () =
  (* we should look at getconf -a *_CACHE_LINE_SIZE here *)
  let cacheline_size = 128 in
  let stride_size = 4096 in
  assert (stride_size >= cacheline_size);
  let tree_node () = Sys.opaque_identity (String.make (stride_size - 8) 'a') in
  let l2cache_size = 1048576 in
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  (*let l2cache_size = 2*67108864 in*)
  (* actually, this is L3 *)
  let l2cache_size =  32*1024 * 1024  in
  let tree_nodes = l2cache_size / (cacheline_size) in
  let init () =
    let r =
      Array.init tree_nodes (fun _ -> (Random.bits (), tree_node ()))
      |> Array.to_seq |> IntMap.of_seq
    in
    Printf.printf "MAP: %d,%d\n%!" (IntMap.cardinal r) (Obj.reachable_words (Obj.repr r) * Sys.word_size / 8);
    r
  and work map =
      let res =
        IntMap.fold
          (fun _ v acc -> acc + Char.code (String.unsafe_get v 0))
          map 0
      in
      let (_ : int) = Sys.opaque_identity res in
      ()
  in
  loop_with_shutdown init work

let word_size_bytes = Sys.word_size / 8

let cacheline_size = 64 (* although seems to be 128 when measured... *)

let stride_size = 128

let stride_size_words = stride_size / word_size_bytes

let make_cycle n =
  let a = Array.init (n / word_size_bytes) Fun.id in
  (* todo: individual alloc, cachielinesize apart? *)
  for i = (Array.length a -1) / stride_size_words downto 1 do
  (* can be any f, what value would result in most cache misses, furthest apart? *)
    let j = Random.int i in (* [0 <= j < i] *)
    let swap = a.(stride_size_words*i) in
    a.(stride_size_words*i) <- a.(stride_size_words*j);
    a.(stride_size_words*j) <- swap
  done;
  Ocaml_intrinsics.Fences.memory_fence ();
  Sys.opaque_identity a  

let max_data_size_log2 = 26

(* should be just below real L1 size *)
let min_cache_size_log2 = 14

(* should be below real cache line size *)
let min_stride_log2 = 4

let operations_log2 = max_data_size_log2 + 1

let rec read_cycle ~data ~remaining ~pos =
  if remaining > 0 then
    let remaining = remaining - 1
    and pos = Array.unsafe_get data pos in
    read_cycle ~data ~remaining ~pos
  else 
    Sys.opaque_identity pos

let cache_misses2 () =
  (* we should look at getconf -a *_CACHE_SIZE here *)
  let l2cache_size = 1048576 in
  (*let l2cache_size = 10240*10240 in*)
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  let init () =
    make_cycle (2*l2cache_size)
    (* TODO: store indexes here, that way speculative execution can't hide latencies... *)
  and work array =
    let remaining = Array.length array in
     let sum = read_cycle ~data:array ~remaining ~pos:0 in
     sum     
  in
  loop_with_shutdown init work

let cache_misses3 () =
  let init () =
    let src = String.make (128*1024*1024) 'x' in
    let dst = Bytes.create (String.length src) in
    src, dst
  in
  let work (src, dst) =
    Bytes.blit_string (Sys.opaque_identity src) 0 dst 0 (String.length src);
    let (_:bytes) = Sys.opaque_identity dst in
    ()
  in
  loop_with_shutdown init work

let cache_misses2' () =
  (* we should look at getconf -a *_CACHE_SIZE here *)
  let l2cache_size = 1048576 in
  (*let l2cache_size = 10240*10240 in*)
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  let init () =
    make_cycle (2*l2cache_size)
    (* TODO: store indexes here, that way speculative execution can't hide latencies... *)
  and work array =
    let remaining = Array.length array in
     let sum = read_cycle ~data:array ~remaining ~pos:0 in
     sum     
  in
  init, work

(*
let cache_misses () =
  (* we should look at getconf -a *_CACHE_SIZE here *)
  let l2cache_size = 1048576 in
  let l2cache_size_words = l2cache_size / word_size_bytes in
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  let init () =
    Array.make_matrix 10240 10240 1
    (* TODO: store indexes here, that way speculative execution can't hide latencies... *)
  and work array =
    let sum = ref 0 in
    for i = 0 to 10239 do
      for j = 0 to 10239 do
        sum := !sum + array.(j).(i) 
      done
    done;
    sum
  in
  loop_with_shutdown init work
*)
