(** Simple, CPU-intensive workloads *)

(** [noalloc_infinite_loop ()] an infinite loop, but it will still contain a poll point,
    where OCaml can react to changing minor heap limit,
    to invoke signal handlers.
 *)
let noalloc_infinite_loop () =
  while true do () done

(** [loop_with_shutdown init f] returns a pair [loop, shutdown], where calling [loop ()] will run until
  [shutdown ()], e.g. in another thread. [loop ()] will call [f arg] repeatedly, where [arg = init ()]
 *)
let loop_with_shutdown init f =
  let arg = init () in
  let stop = Atomic.make false in
  let loop () =
    while not (Atomic.get stop) do f (Sys.opaque_identity arg) done;
    (* prevent [arg] from being completely optimized out if its effects cannot be observed. *)
    let _ = Sys.opaque_identity arg in
    ()
  and shutdown () =
    Atomic.set stop true
  in
  loop, shutdown

let alloc_cycle q =
  let str = String.make 200 (Sys.opaque_identity 'x') in
  Queue.push str q;
  if Queue.length q > 100 then
    Queue.clear q

(** [alloc_cycle ()] allocates and keeps allocated a small amount of data that usually fits in the minor heap,
  and then releases the reference so that it can be garbage collected.
 *)
let alloc_loop () =
  loop_with_shutdown Queue.create alloc_cycle

let copy n =
  let str = String.make n 'x' in
  let copy_n dest =
    Bytes.blit_string (Sys.opaque_identity str) 0 dest 0 (String.length str)
  and alloc () = Bytes.create n in
  loop_with_shutdown alloc copy_n

(** [copy_small ()] copies a small, 16KiB string repeatedly. The copy itself is not interruptible.
 *)
let copy_small () =
  copy (1 lsl 14)

(** [copy_large ()] copies a large, 16MiB string repeatedly. The copy itself is not interruptible,
  with the current implementation in the OCaml runtime.
 *)
let copy_large () =
  copy (1 lsl 24)
