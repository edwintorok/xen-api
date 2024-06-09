(** Derived measures *)

open Bechamel

let[@inline always] [@specialise always] perform witness =
  let (Measure.V (v, (module Impl))) = Measure.prj witness in
  Impl.get v

let div ?(scale = 1.0) ?unit label (num : Measure.witness)
    (denom : Measure.witness) =
  let unit =
    match unit with
    | None ->
        Printf.sprintf "%s/%s" (Measure.unit num) (Measure.unit denom)
    | Some unit ->
        unit
  in
  let module D = struct
    type witness = unit

    let label _ = label

    let unit _ = unit

    let make () = ()

    let load () =
      Measure.load num ;
      try Measure.load denom
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Measure.unload num ;
        Printexc.raise_with_backtrace e bt

    let unload () = Measure.unload denom ; Measure.unload num

    let get () =
      let num = perform num in
      let denom = perform denom in
      scale *. num /. denom
  end in
  let measure = Measure.register (module D) in
  Measure.instance (module D) measure

let percent_div label num denom = div ~scale:100. ~unit:"%" label num denom
