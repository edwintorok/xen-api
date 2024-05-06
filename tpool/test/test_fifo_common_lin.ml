open Test_fifo_common

let test ?(neg = false) ~name (module F : FifoS) =
  let module Spec = MakeLinSpec (F) in
  let module T = Lin_par.Make (Spec) in
  let make = if neg then T.neg_lin_test else T.lin_test in
  make ~count:1000 ~name

(* one test executable per module so that Dune can parallelize tests of different modules *)
let run ?neg ~name (module F : FifoS) =
  QCheck_base_runner.run_tests_main [test ?neg ~name (module F)]
