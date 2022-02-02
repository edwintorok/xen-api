open Opentelemetry.Tracer

module TestProcessor = SimpleProcessor(TestExporter)

let tracer = Provider.get ~name:"test" Provider.global
let () =
  let p = TestProcessor.create tracer @@ TestExporter.create () in
  Provider.register Provider.global (module TestProcessor) p

let test_with_span () =
  let context = Opentelemetry.Context.empty in
  with_span ~name:"span" ~context tracer @@ fun span ->
  ()

let () =
  Alcotest.run "span" [
    "with", ["with", `Quick, test_with_span]
  ]
