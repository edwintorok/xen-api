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

let test_stm_seq (module B: KVBackendLwt) =
  (* no need to use run_in_main: we only have 1 thread here *)

  let lwt f x = Lwt_main.run (f x)
  in
  let lwt2 f x y = Lwt_main.run (f x y)
  in
  let module LwtSeq = struct
    type 'a io = 'a
    type t = B.t
    let name = B.name ^ " (STM lwtseq)"
    let init = B.init
    let cleanup = lwt B.cleanup
    let range = lwt2 B.range
    let put = lwt2 B.put
    let delete_range = lwt2 B.delete_range
  end
  in
  let module Spec = Etcd_spec.MakeSTM(LwtSeq) in
  let module T = STM_sequential.Make(Spec) in
  T.agree_test ~count:100 ~name:LwtSeq.name

let run () =
  Alcotest.run __MODULE__
  [ "qcheck", 
    List.map
    QCheck_alcotest.to_alcotest
    @@
    test (module Memory_backend) ::
    test_stm_seq (module Etcd_backend_json) ::
    test_stm (module Memory_backend)
  ]
