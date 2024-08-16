(*
 * Copyright (C) 2023 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Exporter = struct
  module Unixext = Xapi_stdext_unix.Unixext

  (* Submit JSON/Protobuf to a specified endpoint. *)
  let submit_data ?(is_protobuf=false) ~verbose ~headers url json =
    let content_type = if is_protobuf then Some "application/x-protobuf" else None in
    if json <> "" then
      match Tracing_export.Destination.Http.export ~verbose ?content_type ~headers ~url () json with
      | Error err ->
          Printf.eprintf "Error: %s" (Printexc.to_string err) ;
          exit 1
      | _ ->
          ()

  (** Export traces from file system to a remote endpoint. *)
  let export erase verbose headers src dst =
    let dst = Uri.of_string dst in
    let is_protobuf = String.ends_with ~suffix:".otel" src in
    let submit_json = submit_data ~verbose ~is_protobuf ~headers dst in
    let rec export_file = function
      | path when Sys.is_directory path ->
          (* Recursively export trace files. *)
          Sys.readdir path
          |> Array.iter (fun f -> Filename.concat path f |> export_file)
      | path when Filename.check_suffix path ".zst" ->
          (* Decompress compressed trace file and decide whether to
             treat it as line-delimited or not. *)
          let ( let@ ) = ( @@ ) in
          let@ compressed = Unixext.with_file path [O_RDONLY] 0o000 in
          let@ decompressed = Zstd.Fast.decompress_passive compressed in
          if Filename.check_suffix path ".ndjson.zst" then
            let ic = Unix.in_channel_of_descr decompressed in
            Unixext.lines_iter submit_json ic
          else
            let json = Unixext.string_of_fd decompressed in
            submit_json json
      | path when Filename.check_suffix path ".ndjson" ->
          (* Submit traces line by line. *)
          Unixext.readfile_line submit_json path
      | path ->
          (* Assume any other extension is a valid JSON file. *)
          let json = Unixext.string_of_file path in
          submit_json json
    in
    export_file src ;
    if erase then
      Unixext.rm_rec ~rm_top:true src
end

module Cli = struct
  open Cmdliner

  let src =
    let doc = "The trace file, e.g. /path/to/trace.ndjson" in
    Arg.(required & pos 0 (some string) None (info [] ~docv:"SRC" ~doc))

  let dst =
    let doc =
      "The destination endpoint URL, e.g. http://localhost:9411/api/v2/spans"
    in
    Arg.(required & pos 1 (some string) None (info [] ~docv:"DST" ~doc))

  let headers =
    let doc = "Extra HTTP headers to send" in
    Arg.(value & opt (list string) [] & info ["H"; "header"] ~docv:"HEADER" ~doc)

  let verbose =
    let doc = "Show verbose CURL output" in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)

  let export_term ~erase = Term.(const Exporter.export $ const erase $ verbose $ headers $ src $ dst)

  let cp_cmd =
    let term = export_term ~erase:false in
    let doc = "copy a trace to an endpoint" in
    Cmd.(v (info "cp" ~doc) term)

  let mv_cmd =
    let term = export_term ~erase:true in
    let doc = "copy a trace to an endpoint and erase it afterwards" in
    Cmd.(v (info "mv" ~doc) term)

  let xs_trace_cmd =
    let man =
      [
        `S "DESCRIPTION"
      ; `P "$(mname) is a utility for working with local trace files"
      ]
    in
    let desc =
      let doc = "utility for working with local trace files" in
      Cmd.info "xs-trace" ~doc ~version:"0.1" ~man
    in
    Cmd.group desc [cp_cmd; mv_cmd]

  let main () = Cmd.eval xs_trace_cmd
end

let () = exit (Cli.main ())
