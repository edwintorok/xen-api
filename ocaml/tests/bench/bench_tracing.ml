open Bechamel

(* TODO: need to make start/stop workload a bechamel "measurement",
   that way it runs in the proper place

   just start/stop the atomic var (maybe thread yield on start, to 'kick it'),
   but then the thread needs to stop efficiently, e.g. using cond vars.
   so maybe use cond vars for kicking too?
 *)
let ( let@ ) f x = f x

let trace_test_inner span =
  let@ span =
    Tracing.with_child_trace
      ~attributes:[("foo", "testing")]
      span ~name:__FUNCTION__
  in
  let@ _ =
    Tracing.with_child_trace ~attributes:[("bar", "val")] span ~name:"test"
  in
  Sys.opaque_identity ignore ()

let trace_test_span _ = Tracing.with_tracing ~name:__FUNCTION__ trace_test_inner

let trace_test_off _ = trace_test_inner None

let uuid = "TEST"

let busy_thread stop =
  while not (Atomic.get stop) do
    Sys.opaque_identity (trace_test_span ())
  done

let busy_thread2 stop =
  let a = Array.make 10000 "" in
  while not (Atomic.get stop) do
    (* create work for the GC by continously creating a lot of short lived strings *)
    Sys.opaque_identity (Array.iteri (fun i _ -> a.(i) <- String.make 2 'x') a)
  done

let allocate ~use_busy () =
  Tracing.TracerProvider.create ~enabled:true ~attributes:[] ~endpoints:[]
    ~name_label:__MODULE__ ~uuid ;
  let stop = Atomic.make false in
  let workload =
    if use_busy >= 1 then
      Some (stop, Thread.create busy_thread stop)
    else
      None
  in
  let workload2 =
    if use_busy >= 2 then
      Some (stop, Thread.create busy_thread2 stop)
    else
      None
  in
  (workload, workload2, Tracing_export.main ())

let free (workload, workload2, t) =
  let () =
    match workload with
    | Some (stop, t) ->
        Atomic.set stop true ; Thread.join t
    | None ->
        ()
  in
  let () =
    match workload2 with
    | Some (stop, t) ->
        Atomic.set stop true ; Thread.join t
    | None ->
        ()
  in
  Tracing.TracerProvider.destroy ~uuid ;
  Tracing_export.flush_and_exit () ;
  Thread.join t

let test_tracing_on ~use_busy ~name f =
  Test.make_with_resource ~name ~allocate:(allocate ~use_busy) ~free Test.uniq f

let benchmarks =
  Test.make_grouped ~name:"tracing"
    [
      Test.make ~name:"overhead(off)" (Staged.stage trace_test_off)
    ; test_tracing_on ~use_busy:0 ~name:"overhead(on, no span, no workload)"
        (Staged.stage trace_test_off)
    ; test_tracing_on ~use_busy:1 ~name:"overhead(on, no span, workload)"
        (Staged.stage trace_test_off)
    ; test_tracing_on ~use_busy:2 ~name:"overhead(on, no span, workload2)"
        (Staged.stage trace_test_off)
    ; test_tracing_on ~use_busy:0 ~name:"overhead(on, create span, no workload)"
        (Staged.stage trace_test_span)
    ; test_tracing_on ~use_busy:1 ~name:"overhead(on, create span, workload)"
        (Staged.stage trace_test_span)
    ; test_tracing_on ~use_busy:2 ~name:"overhead(on, create span, workload2)"
        (Staged.stage trace_test_span)
    ]

let () =
  Tracing.Spans.set_max_spans 100_000_000;
  Tracing.Spans.set_max_traces 100_000_000;
  Bechamel_simple_cli.cli benchmarks
