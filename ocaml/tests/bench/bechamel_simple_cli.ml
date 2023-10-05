(* based on bechamel example code *)
open Bechamel
open Toolkit

let instances = Instance.[monotonic_clock; minor_allocated; major_allocated]

let cfg =
  (* stabilize:true would be the default but it measures GC stabilization time as part of the function
     runtime, leading to about 10x as much time measured than without.
     It is also confusing for flamegraphs because the GC will show up much more frequently than in reality
     due to the thousands of repeated calls.
  *)
  Benchmark.cfg
    ~quota:Time.(second 5.0)
    ~start:10 ~stabilize:false ~compaction:false ()

let benchmark tests = Benchmark.all cfg instances tests

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:[|Measure.run|]
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  (Analyze.merge ols instances results, raw_results)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let show_results results =
  let () =
    List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances
  in

  let results, _ = results |> analyze in

  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image

let cli tests =
  Format.printf "@,Running benchmarks@." ;
  let results = tests |> benchmark in
  show_results results

let results = Hashtbl.create 47

open Alcotest.V1

let to_alcotest tests =
  let open Bechamel in
  let name = Bechamel.Test.name tests in
  let tests =
    tests
    |> Test.elements
    |> List.map @@ fun test ->
       let name = Test.Elt.name test in
       test_case name `Slow @@ fun () ->
       Hashtbl.add results name @@ Benchmark.run cfg instances test
  in
  (name, tests)

let alcotest_cli tests =
  let name = Bechamel.Test.name tests in
  (* Alcotest exits when filtering is used even with 'and_exit:false', so setup an at_exit handler to show results *)
  let () = at_exit (fun () -> show_results results) in
  run ~show_errors:true name [to_alcotest tests]
