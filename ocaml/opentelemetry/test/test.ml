open Opentelemetry.Tracer
module Proto = Opentelemetry.Proto

let () = Logs.set_level ~all:true (Some Logs.Debug)

let encoder, decoder =
  Ocaml_protoc_plugin.Service.make_client_functions
    Proto.Collector.Trace.V1.TraceService.export

let send_spans resource_spans f =
  Proto.Collector.Trace.V1.ExportTraceServiceRequest.make ~resource_spans ()
  |> encoder
  |> Ocaml_protoc_plugin.Writer.contents
  |> f

let to_result = function
  | Ok v ->
      Ok v
  | Error e ->
      Error (`Msg (Ocaml_protoc_plugin.Result.show_error e))

let parse_reply reply =
  reply |> Ocaml_protoc_plugin.Reader.create |> decoder |> to_result

let parse_reply_exn reply =
  match parse_reply reply with Ok v -> v | Error (`Msg m) -> failwith m

module TestExporter = struct
  type t = unit

  let create () = ()

  let sdecoder, sencoder =
    Ocaml_protoc_plugin.Service.make_service_functions
      Proto.Collector.Trace.V1.TraceService.export

  let handler s =
    match s |> Ocaml_protoc_plugin.Reader.create |> sdecoder |> to_result with
    | Error (`Msg m) ->
        failwith m
    | Ok spans ->
        Logs.debug (fun m ->
            m "got: %a" Proto.Collector.Trace.V1.ExportTraceServiceRequest.pp
              spans
        ) ;
        (* TODO: pp+log *)
        Proto.Collector.Trace.V1.ExportTraceServiceResponse.make ()
        |> sencoder
        |> Ocaml_protoc_plugin.Writer.contents

  let export () spans =
    send_spans spans handler |> parse_reply_exn ;
    true

  let force_flush _ = ()

  let shutdown _ = ()
end

module TestProcessor = SimpleProcessor (TestExporter)

let tracer = Provider.get ~name:"test" Provider.global

let () =
  let p = TestProcessor.create tracer @@ TestExporter.create () in
  Provider.register Provider.global (module TestProcessor) p

let test_with_span () =
  let context = Opentelemetry.Context.empty in
  with_span ~name:"span" ~context tracer @@ fun span -> ()

let () = Alcotest.run "span" [("with", [("with", `Quick, test_with_span)])]
