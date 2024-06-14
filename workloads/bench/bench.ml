open Bechamel
open Simple_workloads

let instance_of_atomic ~name ~unit atomic =
  let module M = struct
    type witness = int Atomic.t

    let name = name
    let label _ = name
    let unit _ = unit
    let make () = atomic
    let load = ignore
    let unload = ignore
    let get t =
      Atomic.get t |> float_of_int
  end
  in
  let measure = Measure.register (module M) in
  Measure.instance (module M) measure

let yields_name = "yields"
let yields = instance_of_atomic ~name:yields_name ~unit:"yield" Operations.Yield.count

let make ~workers ~name ~args ~worker ~allocate ~free test =
  let allocate n =
    Workers.allocate worker workers, allocate n
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
