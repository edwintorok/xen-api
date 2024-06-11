open Bechamel

type t = { predictors: string array ; measure: Measure.witness }

let make ~predictors measure = { predictors; measure }

let cumulative measure = make ~predictors:[|Measure.run|] measure

let one = Toolkit.One.label ()

let div ~unit ~scale label num denom =
  let module Div = struct
    type witness =
      { num: Measure.witness * Measure.value
      ; denom: Measure.witness * Measure.value
      }

    let label _ = label
    let unit _ = unit
    let make () =
      let num = num, Measure.prj num
      and denom = denom, Measure.prj denom in
      { num; denom }

    let load w =
      Measure.load (fst w.num);
      Measure.load (fst w.denom)

    let unload w =
      Measure.unload (fst w.denom);
      Measure.unload (fst w.num)

    let get_value (_, Measure.V (v, (module Impl))) =
      Impl.get v
    
    let get w =
      let num = get_value w.num in
      let denom = get_value w.denom in
      scale *. num /. denom      
  end in
  let measure = Measure.register (module Div) in
  let witness = Measure.instance (module Div) measure in
  make ~predictors:[|one|] witness

let percentage = div ~unit:"%" ~scale:100.

let ols { predictors; measure } ~bootstrap benchmark =
  let ols = Analyze.ols ~r_square:true ~bootstrap ~predictors in
  Analyze.one ols measure benchmark

