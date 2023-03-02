open Service

let ( let* ) = Result.bind

module Make (Svc : S) = struct
  let src = Logs.Src.create __MODULE__

  include (val Logs.src_log src)

  include Svc

  let fdebug id funct k =
    debug (fun m -> k (fun fmt -> m ("%s(%a): " ^^ fmt) funct Uuidm.pp id))

  let with_validated f id config =
    let* () = validate id config in
    f id config

  let start = with_validated Svc.start

  let reload = with_validated Svc.reload

  let valid_config_or_none id = function
    | None ->
        None
    | Some config -> (
      match validate id config with
      | Ok () ->
          Some config
      | Error e ->
          fdebug id __FUNCTION__ (fun m ->
              m "Configuration not valid, performing force stop: %a" pp_error e
          ) ;
          None
    )

  let stop id config_opt = config_opt |> valid_config_or_none id |> Svc.stop id
end
