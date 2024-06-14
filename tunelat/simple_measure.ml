(* Measure the speed of an operation in a very simple, and robust way.
   For more detailed measurements see bench/ and bechamel
*)

let measure ?(n=1001) ?(inner=10) f counter =
   assert (n > 70);
   let measure_inner _ =
      let m = Mtime_clock.counter () in
      let v0 = Atomic.get counter in
      for _ = 1 to inner do
         Sys.opaque_identity (f ());
      done;
      let elapsed = Mtime_clock.count m in
      let v1 = Atomic.get counter in
      (elapsed |> Mtime.Span.to_float_ns) *. 1e-9 /. (float @@ v1 - v0)
   in
   let measurements = Array.init n measure_inner in 
   Array.sort Float.compare measurements;
   let median = measurements.(n / 2) in
   (* perfeval Table A. 1 *)
   let n = float n in
   let d = 0.98 *. sqrt n in
   let lo = n /. 2. -. d |> Float.to_int
   and hi = n /. 2. +. 1. +. d |> Float.ceil |> Float.to_int in
   measurements.(lo - 1), median, measurements.(hi - 1)


