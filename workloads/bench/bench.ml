open Bechamel
open Simple_workloads

module Yields = struct
  type witness = int Atomic.t

  let name = "yields"
  let label _ = name
  let unit _ = ""
  let make () = Operations.Yield.count
  let load = ignore
  let unload = ignore
  let get t = Atomic.get t |> float_of_int
end

let yields =
  let measure = Measure.register (module Yields) in
  Measure.instance (module Yields) measure

let make ~name ~args ~worker ~allocate ~free test =
  let allocate n =
    Workers.allocate worker n, allocate n
  and free (workers, t) =
    Workers.free workers;
    free t
  in
  Test.make_indexed_with_resource ~name ~args Test.uniq
    ~allocate ~free @@ fun n ->
  let test = test n in
  Staged.stage @@ fun ((ready, _), t) ->
  Workers.StartStop.start ready;
  test t
