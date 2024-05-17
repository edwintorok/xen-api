module Sys = Private.Lib.Sys

let ( let* ) = Lwt.bind

let test_content_rountrip =
  let test () =
    let contents = "yes" in
    let path = Filename.temp_file "" "" in
    let* () = Sys.save ~contents path in
    let* result = Sys.read_file_contents path in
    Alcotest.(check string) "Write and read roundtrip" contents result ;
    Lwt.return ()
  in
  [("Write and read file", `Quick, test)]

let test_sys = [("Sys", test_content_rountrip)]

let tests = test_sys

let () = Lwt_main.run @@ Alcotest_lwt.run "xapi-storage-script lib" tests
