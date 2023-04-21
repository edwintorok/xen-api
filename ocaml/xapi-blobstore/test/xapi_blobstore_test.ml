module XB = Spec.MakeDirect(Xapi_blobstore_client.Xapiblob)

let tests ~count =
  [ (module Kv_memory)
  ; (module XB)
  ]
  |> List.concat_map @@ fun m ->
  let _name, tests = Spec.tests ~count m in
  tests

let () =
  QCheck_base_runner.run_tests_main @@
  tests ~count:100