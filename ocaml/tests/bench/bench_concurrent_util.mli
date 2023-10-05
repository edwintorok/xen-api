(** Helpers for measuring concurrent or parallel code *)

val parallel_c_work : int -> unit
(** [parallel_c_work ms] releases the OCaml runtime lock and performs a fixed length operation for [ms] milliseconds.

  This can be useful for testing how efficient the various thread dispatch mechanisms are.
 *)

val test_concurrently :
     ?threads:int list
  -> allocate:(unit -> 'a)
  -> free:('a -> unit)
  -> name:string
  -> ('a -> unit) Bechamel.Staged.t
  -> Bechamel.Test.t
(** [test_concurrently ?threads ~allocate ~free ~name run] is a benchmark for [run].
  [n] threads are created.
  [allocate ()] is called to allocate a resource, which is reused across multiple invocations of [run] from the same thread,
  and finally [free resource] is called.
  [threads] specifies the number of threads to test with.
  Only the execution time of [run] (plus thread synchronization overhead) is measured, and not [allocate] and [free].    
*)
