open Etcd_service_types

let test (module B : KVBackendDirect) =
  let module Spec = Etcd_spec.Make(B) in
  let module T = Lin_thread.Make(Spec) in
  T.lin_test ~count:100 ~name:B.name

let run () =
  QCheck_base_runner.run_tests_main
  [ test (module Memory_backend)
  ]
