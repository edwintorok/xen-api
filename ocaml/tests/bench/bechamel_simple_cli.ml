(* based on bechamel example code *)
open Bechamel
open Toolkit
open Cmdliner

let span =
  let parse s =
    try Ok (s |> Float.of_string |> Bechamel.Time.second)
    with e -> Error (Printexc.to_string e)
  in
  let print ppf t =
    Format.fprintf ppf "%fs"
      ((t |> Bechamel.Time.span_to_uint64_ns |> Int64.to_float) *. 1e-9)
  in
  Arg.conv' ~docv:"SECONDS" (parse, print)

let measures =
  let witness =
    Instance.
      [
        minor_allocated
      ; major_allocated
      ; promoted
      ; compaction
      ; minor_collection
      ; major_collection
      ; monotonic_clock
      ]
    (* cannot add the perf measures because they instantly fail when not root even when we're not using them,
       just referencing their name causes the module to be initialized
    *)
    |> List.map (fun w -> (Measure.label w, w))
    |> Arg.enum
  in
  Arg.(
    value
    & opt (list witness)
        Instance.[monotonic_clock; minor_allocated; major_allocated]
    & info ["measures"] ~doc:"What measurements to record" ~docv:"MEASURE"
  )

let cfg =
  let limit =
    Arg.(
      value
      & opt (some int) None
      & info ["limit"] ~doc:"maximum number of samples allowed" ~docv:"SAMPLES"
    )
  and quota =
    Arg.(
      value
      & opt span (Time.second 5.0)
      & info ["quota"] ~doc:"maximum time allowed" ~docv:"SECONDS"
    )
  and kde =
    Arg.(
      value
      & opt (some int) None
      & info ["kde"]
          ~doc:
            "additional measurements for KDE and histogram display. Actual \
             time limit will be 2*quota"
    )
  and stabilize =
    Arg.(
      value
      & opt bool false
      & info ["stabilize"]
          ~doc:
            "stabilize the GC before each run. Beware that although \
             measurements will be more stable they may sometimes slow down \
             even 10x the measured value."
    )
  and start =
    Arg.(
      value
      & opt (some int) None
      & info ["start"] ~doc:"the first value of the run metric"
    )
  and compaction =
    Arg.(
      value
      & opt bool false
      & info ["compaction"]
          ~doc:"whether to enable GC compaction during the benchmark"
    )
  in
  let cfg limit quota kde stabilize start compaction =
    Benchmark.cfg ?limit ~quota ~kde ~stabilize ?start ~compaction ()
  in
  Term.(const cfg $ limit $ quota $ kde $ stabilize $ start $ compaction)

let analysis =
  let bootstrap =
    Arg.(
      value
      & opt int 0
      & info ["bootstrap"]
          ~doc:
            "how many times to resample the measurements (needed for \
             confidence interval calculations)"
    )
  in
  let analyze bootstrap =
    Analyze.ols ~r_square:true ~bootstrap ~predictors:[|Measure.run|]
  in
  Term.(const analyze $ bootstrap)

let analyze (_, measures, analysis, _) raw_results =
  let results =
    List.map (fun measure -> Analyze.all analysis measure raw_results) measures
  in
  (Analyze.merge analysis measures results, raw_results)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

type output = Tty | Json | Text

let dump_hashtbl name pp_elt =
  Fmt.Dump.iter_bindings Hashtbl.iter Fmt.(any name) Fmt.string pp_elt

let dump_measure ppf m =
  Format.fprintf ppf "%s: %s" (Measure.label m) (Measure.unit m)

let show_results ((_, measures, _, (output, json_file)) as t) results =
  let results, raw_results = analyze t results in
  let () =
    match output with
    | Tty ->
        let () =
          List.iter
            (fun i -> Bechamel_notty.Unit.add i (Measure.unit i))
            measures
        in

        let window =
          match winsize Unix.stdout with
          | Some (w, h) ->
              {Bechamel_notty.w; h}
          | None ->
              {Bechamel_notty.w= 80; h= 1}
        in
        img (window, results) |> eol |> output_image
    | Json ->
        ()
    | Text ->
        Format.printf "Measures: %a@." Fmt.Dump.(list dump_measure) measures ;

        Format.printf "%a@."
          (dump_hashtbl "results" (dump_hashtbl "result" Analyze.OLS.pp))
          results
  in
  let file = open_out json_file in
  let finally () = close_out file in
  Fun.protect ~finally @@ fun () ->
  let open Bechamel_js in
  match
    emit ~dst:(Channel file)
      (fun _ -> Ok ())
      ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      (results, raw_results)
  with
  | Ok () ->
      Format.printf "Saved JSON results to %s@." json_file
  | Error (`Msg err) ->
      invalid_arg err

let results = Hashtbl.create 47

let benchmark_cli =
  let open Cmdliner in
  let output =
    Arg.(
      value
      & opt (enum [("tty", Tty); ("json", Json); ("text", Text)]) Tty
      & info ["output"]
    )
  in
  let json_file =
    let self = Sys.executable_name |> Filename.basename in
    Arg.(
      value
      & opt string (self ^ ".json")
      & info ["output-json"] ~doc:"JSON file to write results to"
    )
  in
  let tuple a b c output json_file =
    let t = (a, b, c, (output, json_file)) in
    let () = at_exit (fun () -> show_results t results) in
    t
  in
  Term.(const tuple $ cfg $ measures $ analysis $ output $ json_file)

open Alcotest.V1

let to_alcotest tests =
  let open Bechamel in
  let name = Bechamel.Test.name tests in
  let tests =
    tests
    |> Test.elements
    |> List.map @@ fun test ->
       let name = Test.Elt.name test in
       test_case name `Slow @@ fun (cfg, measures, _, _) ->
       Hashtbl.add results name @@ Benchmark.run cfg measures test
  in
  (name, tests)

let alcotest_cli tests =
  let name = Bechamel.Test.name tests in
  (* Alcotest exits when filtering is used even with 'and_exit:false', so setup an at_exit handler to show results *)
  run_with_args ~show_errors:true name benchmark_cli [to_alcotest tests]
