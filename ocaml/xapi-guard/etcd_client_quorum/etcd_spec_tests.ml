open Etcd_service_types

let test (module B : KVBackendDirect) =
  let module Spec = Etcd_spec.Make(B) in
  let module T = Lin_thread.Make(Spec) in
  T.lin_test ~count:100 ~name:B.name
  
let test_stm (module B: KVBackendDirect) =
  let module Spec = Etcd_spec.MakeSTM(B) in
  let module T1 = STM_sequential.Make(Spec) in
  let module T2 = STM_thread.Make(Spec) in
  [ T1.agree_test ~count:100 ~name:(B.name ^ " STM seq")
  ; T2.agree_test_conc ~count:100 ~name:(B.name ^ " STM thread")
  ]

let run () =
  QCheck_base_runner.run_tests_main
  @@
  test (module Memory_backend) :: test_stm (module Memory_backend)
