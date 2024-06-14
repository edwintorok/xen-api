(* based on bechamel example code *)
open Bechamel
open Toolkit

let benchmark ~instances tests =
  let cfg = Benchmark.cfg ~start:2 ~stabilize:false () in
  Benchmark.all cfg instances tests

let analyze ~instances ~predictors raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors
  in
  let instances =
    List.filter (fun instance -> not @@ Array.mem (Measure.label instance) predictors) instances
  in
  let results =
    List.map (fun instance ->
       let r = Analyze.all ols instance raw_results in
       Hashtbl.iter (fun k v -> Fmt.epr "%s: %a@," k Analyze.OLS.pp v) r;
       r
    ) instances
  in
  (Analyze.merge ols instances results, raw_results)

let img ~predictor (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor results
  
open Notty_unix

let cli ?(predictors=[|Measure.run|]) ?(instances=[Instance.monotonic_clock]) tests =
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances;
  Format.printf "@,Running benchmarks@." ;
  let results, _ = tests |> benchmark ~instances |> analyze ~instances ~predictors in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  predictors |> Array.iter @@ fun predictor ->
  (* todo: merge notty image *)
  img  ~predictor (window, results) |> eol |> output_image
