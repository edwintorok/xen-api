(* Tunes parameters for the hardware and kernel that we are currently running on. *)

module D = Debug.Make (struct let name = "xshwtune" end)

let () =
  Debug.log_to_stdout () ;
  let recommended = Xapi_timeslice.Recommended.measure () in
  D.debug "Recommended OCaml timeslice: %.6fs" recommended ;

  let itimer_min =
    Sys.set_signal Sys.sigvtalrm Sys.Signal_ignore ;
    let _ =
      Unix.setitimer Unix.ITIMER_VIRTUAL
        Unix.{it_value= 1e-6; it_interval= 1e-6}
    in
    let actual = Unix.getitimer Unix.ITIMER_VIRTUAL in
    actual.Unix.it_value
  in

  D.debug "POSIX itimer granularity: %.6fs" itimer_min ;
  let recommended = Float.max itimer_min recommended in

  (* just in case something goes very wrong, ensure it is not too small or big *)
  let recommended = recommended |> Float.max 0.001 |> Float.min 0.050 in
  D.debug "Adjusted timeslice: %.6fs" recommended ;

  (* Use consistent rounding in debug messages and conf file,
     by converting to string in a single place.
  *)
  let recommended = Printf.sprintf "%.3f" recommended in

  D.info "OCaml timeslice: %s" recommended ;

  let write_conf path =
    Out_channel.with_open_text path @@ fun ch ->
    Printf.fprintf ch "timeslice=%s" recommended
  in

  Array.iteri
    (fun i arg ->
      if i > 0 then
        write_conf arg
    )
    Sys.argv
