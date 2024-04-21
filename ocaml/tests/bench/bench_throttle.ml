open Bechamel

let get_rusage () = Rusage_thread.getrusage_thread_ns () |> Int64.to_int

let benchmarks =
  Test.make_grouped ~name:"Rusage_thread"
    [Test.make ~name:"overhead" (Staged.stage get_rusage)]

let () = Bechamel_simple_cli.cli benchmarks
