(* based on bechamel example code *)
open Bechamel
open Toolkit

let instances = Instance.[monotonic_clock; Bench.yields]

let benchmark tests =
  let cfg = Benchmark.cfg ~stabilize:false () in
  Benchmark.all cfg instances tests

let analyze ~predictors raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  (Analyze.merge ols instances results, raw_results)

let () =
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let cli ?(predictors=[|Measure.run|]) tests =
  Format.printf "@,Running benchmarks@." ;
  let results, _ = tests |> benchmark |> analyze ~predictors in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image
