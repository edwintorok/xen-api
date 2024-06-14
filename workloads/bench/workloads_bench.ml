open Simple_workloads

let test =
  Bench.make ~name:"yield overhead" ~args:[1]
    ~workers:512
    ~worker:Operations.Yield.worker
    ~allocate:ignore
    ~free:ignore
    @@ fun _ _ ->
    Operations.Yield.perform ()

let () =
  Bechamel_simple_cli.cli ~predictors:[|(*Bechamel.Measure.run;*)Bench.Yields.name;|] test
