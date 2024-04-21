open Bechamel

let get_rusage () = Rusage_thread.getrusage_thread_ns () |> Int64.to_int

let test_rusage = Throttle.Rusage.make "test"

let measure_rusage_overhead () =
  Throttle.Rusage.measure_rusage test_rusage ignore ()

let benchmarks =
  Test.make_grouped ~name:"Rusage_thread"
    [
      Test.make ~name:"overhead" (Staged.stage get_rusage)
    ; Test.make ~name:"measure overhead" (Staged.stage measure_rusage_overhead)
    ]

let () = Bechamel_simple_cli.cli benchmarks
