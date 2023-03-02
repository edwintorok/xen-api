(* do not allow exceptions (other than perhaps Out_of_memory) to escape. *)

let src = Logs.Src.create __MODULE__

include (val Logs.src_log src)

module Make (Svc : Service.S) = struct
  let open_error e = Rresult.R.open_error_exn_trap (Result.error e)

  let trap_exn ~pp_value f v =
    let pp ppf exn_trap =
      Fmt.pf ppf "Caught exception when called with %a" pp_value v ;
      Rresult.R.pp_exn_trap ppf exn_trap
    in
    Rresult.R.trap_exn f v
    (* log it now at debug level, just in case it gets lost later *)
    |> on_error ~level:Logs.Debug ~use:open_error ~pp

  let trap_exn2 ~pp_value f v1 v2 =
    trap_exn ~pp_value (fun (v1, v2) -> f v1 v2) (v1, v2)

  module Config = struct
    include Svc.Config

    let of_dict = trap_exn ~pp_value:Astring.String.Map.dump_string_map of_dict

    let dump = Fmt.using to_dict Astring.String.Map.dump_string_map
  end

  let name = Svc.name

  let pp_value =
    Fmt.Dump.(record [field "id" fst Uuidm.pp; field "config" snd Config.dump])

  let validate = trap_exn2 ~pp_value Svc.validate

  let start = trap_exn2 ~pp_value Svc.start

  let reload = trap_exn2 ~pp_value Svc.reload

  let pp_value =
    Fmt.Dump.(
      record [field "id" fst Uuidm.pp; field "config" snd (option Config.dump)]
    )

  let stop = trap_exn2 ~pp_value Svc.stop

  let is_running id ~check_health =
    id
    |> trap_exn ~pp_value:Uuidm.pp @@ fun id -> Svc.is_running id ~check_health
end
