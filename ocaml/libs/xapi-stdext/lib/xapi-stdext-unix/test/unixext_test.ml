open QCheck2
open Xapi_stdext_unix
open Xapi_fd_test

let expect_string ~expected ~actual =
  if not (String.equal expected actual) then
    Test.fail_reportf "Data sent and observed do not match: %S <> %S" expected
      actual

let expect_amount ~expected observation =
  let open Observations in
  let actual = String.length observation.data in
  if expected <> actual then
    Test.fail_reportf
      "Amount of data available and transferred does not match: %d <> %d;@,%a"
      expected actual pp observation

let skip_blk = function
  | Unix.S_BLK ->
      if Unix.geteuid () <> 0 then
        QCheck2.assume_fail ()
  | _ ->
      ()

let skip_dirlnk = function
  | Unix.S_DIR | Unix.S_LNK ->
      QCheck2.assume_fail ()
  | _ ->
      ()

(*
let pp_pair =
  let open Observations in
  Fmt.(record
    [ field "read" (fun t -> t.read) pp
    ; field "write" (fun t -> t.write) pp
    ; field "elapsed" (fun t -> t.elapsed) Mtime.Span.pp
   ]
  )
*)

let test_time_limited_write =
  let gen = Gen.tup2 Generate.t Generate.timeouts
  and print = Print.tup2 Generate.print Print.float in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout) ->
  skip_blk behaviour.kind ;
  skip_dirlnk behaviour.kind ;
  try
    let test_elapsed = ref Mtime.Span.zero in
    let test wrapped_fd =
      let len = behaviour.size in
      let buf = String.init len (fun i -> Char.chr (i mod 255)) in
      let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
      Unix.set_nonblock fd ;
      let dt = Mtime_clock.counter () in
      let deadline = Unix.gettimeofday () +. timeout in
      let finally () = test_elapsed := Mtime_clock.count dt in
      Fun.protect ~finally (fun () ->
          Unixext.time_limited_write_substring fd len buf deadline
      ) ;
      buf
    in
    (*Printf.eprintf "testing write: %s\n%!" (print (behaviour, timeout)) ;*)
    let observations, result = Generate.run_wo behaviour ~f:test in
    let () =
      let open Observations in
      let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
      if elapsed_s > timeout +. 0.5 then
        Test.fail_reportf
          "Function duration significantly exceeds timeout: %f > %f; %s"
          elapsed_s timeout
          (Fmt.to_to_string Fmt.(option pp) observations.Observations.read) ;
      match (observations, result) with
      | {read= Some read; _}, Ok expected ->
          (* expected is the input given to [time_limited_write_substring] *)
          expect_amount ~expected:(String.length expected) read ;
          expect_string ~expected ~actual:read.data
      | {read= Some read; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
          let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
          if elapsed_s < timeout then
            Test.fail_reportf "Timed out earlier than requested: %f < %f"
              elapsed_s timeout ;
          let actual = String.length read.data in
          if actual >= behaviour.size then
            Test.fail_reportf "Timed out, but transferred enough data: %d >= %d"
              actual behaviour.size
      | ( {read= Some read; _}
        , Error (`Exn_trap (Unix.Unix_error (Unix.EPIPE, _, _), _)) ) ->
          if String.length read.data = behaviour.size then
            Test.fail_reportf
              "Transferred exact amount, shouldn't have tried to send more: %d"
              behaviour.size
      | {read= None; _}, _ ->
          ()
      | _, Error (`Exn_trap (e, bt)) ->
          Printexc.raise_with_backtrace e bt
    in
    true
  with e ->
    Format.eprintf "Error: %a@." Fmt.exn_backtrace
      (e, Printexc.get_raw_backtrace ()) ;
    false

let test_time_limited_read =
  let gen = Gen.tup2 Generate.t Generate.timeouts
  and print = Print.tup2 Generate.print Print.float in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout) ->
  (* Format.eprintf "Testing %s@." (print (behaviour, timeout)); *)
  skip_blk behaviour.kind ;
  skip_dirlnk behaviour.kind ;
  let test_elapsed = ref Mtime.Span.zero in
  let test wrapped_fd =
    let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
    Unix.set_nonblock fd ;
    let dt = Mtime_clock.counter () in
    let deadline = Unix.gettimeofday () +. timeout in
    let finally () = test_elapsed := Mtime_clock.count dt in
    Fun.protect ~finally (fun () ->
        Unixext.time_limited_read fd behaviour.size deadline
    )
  in
  (*Printf.eprintf "testing: %s\n%!" (print (behaviour, timeout)) ;*)
  let observations, result =
    let buf = String.init behaviour.size (fun i -> Char.chr (i mod 255)) in
    Generate.run_ro behaviour buf ~f:test
  in
  let () =
    let open Observations in
    let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
    if elapsed_s > timeout +. 0.5 then
      Test.fail_reportf
        "Function duration significantly exceeds timeout: %f > %f; %s" elapsed_s
        timeout
        (Fmt.to_to_string Fmt.(option pp) observations.Observations.write) ;
    (* Format.eprintf "Result: %a@." (Fmt.option Observations.pp) observations.write;*)
    match (observations, result) with
    | {write= Some write; _}, Ok actual ->
        expect_amount ~expected:(String.length actual) write ;
        expect_string ~expected:write.data ~actual
    | {write= Some _; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
        let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
        if elapsed_s < timeout then
          Test.fail_reportf "Timed out earlier than requested: %f < %f"
            elapsed_s timeout
    | ( {write= Some write; _}
      , Error (`Exn_trap (Unix.Unix_error (Unix.EPIPE, _, _), _)) ) ->
        if String.length write.data = behaviour.size then
          Test.fail_reportf
            "Transferred exact amount, shouldn't have tried to send more: %d"
            behaviour.size
    | {write= None; _}, _ ->
        ()
    | _, Error (`Exn_trap (e, bt)) ->
        Printexc.raise_with_backtrace e bt
  in
  true

let tests = [test_time_limited_write; test_time_limited_read]

let () =
  (* avoid SIGPIPE *)
  let (_ : Sys.signal_behavior) = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  QCheck_base_runner.run_tests_main tests
