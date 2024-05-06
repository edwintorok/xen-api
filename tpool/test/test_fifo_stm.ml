open Test_fifo_common

let test ?(neg = false) ~name (module F : FifoS) =
  let count = 1000 in
  let module Spec = MakeSTMSpec (F) in
  let module T1 = STM_sequential.Make (Spec) in
  let module T2 = STM_par.Make (Spec) in
  let par_test = if neg then T2.neg_agree_test else T2.agree_test in
  [ T1.agree_test ~count
      ~name:(Printf.sprintf "sequential STM %s tests" name)
  ; par_test ~count ~name:(Printf.sprintf "parallel STM %s tests" name) ]

let () =
  [ test ~neg:true ~name:"Fifo_unsafe" (module Test_internal.Fifo_unsafe)
    (* ; test ~name:"Fifo_locked" (module Test_internal.Fifo_locked)
       ; test ~name:"Fifo_lockfree" (module Test_internal.Fifo_lockfree) *) ]
  |> List.concat |> QCheck_base_runner.run_tests_main
