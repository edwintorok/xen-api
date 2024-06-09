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
  let cacheline_size = 64 in
  let tree_node () = Sys.opaque_identity (String.make cacheline_size 'a') in
  let l2cache_size = 1048576 in
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  (*let l2cache_size = 2*67108864 in*)
  let l2cache_size = 20*34635776 in
  let tree_nodes = l2cache_size / (2*cacheline_size) in
  let init () =
    let r =
      Array.init tree_nodes (fun _ -> (Random.bits (), tree_node ()))
      |> Array.to_seq |> IntMap.of_seq
    in
    Printf.printf "MAP: %d,%d\n%!" (IntMap.cardinal r) (Obj.reachable_words (Obj.repr r) * Sys.word_size / 8);
    r
  and work map =
    for i = 0 to cacheline_size - 1 do
      let res =
        IntMap.fold
          (fun _ v acc -> acc + Char.code (String.unsafe_get v i))
          map 0
      in
      let (_ : int) = Sys.opaque_identity res in
      ()
    done
  in
  loop_with_shutdown init work

let word_size_bytes = Sys.word_size / 8

let make_cycle n =
  let a = Array.init (64*n) Fun.id in
  (* todo: individual alloc, cachielinesize apart? *)
  for i = n - 1 downto 1 do
  (* can be any f, what value would result in most cache misses, furthest apart? *)
    let j = Random.int i in (* [0 <= j < i] *)
    let swap = a.(64*i) in
    a.(64*i) <- a.(64*j);
    a.(64*j) <- swap
  done;
  a  

let rec cycle a sum i =
  if i <> 0 then
    let next = Sys.opaque_identity Array.unsafe_get a i in
    cycle a (sum + next) next

let cache_misses () =
  (* we should look at getconf -a *_CACHE_SIZE here *)
  let l2cache_size = 1048576 in
  (*let l2cache_size = 10240*10240 in*)
  let l2cache_size_words = l2cache_size / word_size_bytes in
  (* TODO *)
  (* TODO: l1+l2+l3 size..? look at cache miss rate if we exceed size
   *)
  let init () =
    make_cycle l2cache_size_words
    (* TODO: store indexes here, that way speculative execution can't hide latencies... *)
  and work array =
     let sum = cycle array 0 array.(0) in
     sum     
  in
  loop_with_shutdown init work

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
