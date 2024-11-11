open Bechamel

let get_rusage () = Rusage_thread.getrusage_thread_ns () |> Int64.to_int

let test_rusage = Throttle.Rusage.make "test"

let measure_rusage_overhead () =
  Throttle.Rusage.measure_rusage test_rusage ignore ()

let test_controller =
  Throttle.Controller.make ~max_cpu_usage:0.1 ~delay_before:0. ~delay_between:0.

let test_limit = Throttle.Limit.make test_rusage test_controller

let measure_limit_overhead () = Throttle.Limit.with_limit test_limit ignore ()

let measure_limit_update () = Throttle.Limit.update test_limit

let benchmarks =
  Test.make_grouped ~name:"Rusage_thread"
    [
      Test.make ~name:"overhead" (Staged.stage get_rusage)
    ; Test.make ~name:"measure overhead" (Staged.stage measure_rusage_overhead)
    ; Test.make ~name:"Limit.with_limit overhead"
        (Staged.stage measure_limit_overhead)
    ; Test.make ~name:"Limit.update overhead" (Staged.stage measure_limit_update)
    ]

let () = Bechamel_simple_cli.cli benchmarks
