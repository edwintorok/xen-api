(* do not allow exceptions (other than perhaps Out_of_memory) to escape. *)

module Make (Svc : Service.S) = struct
  let src = Logs.Src.create __MODULE__

  include (val Logs.src_log src)

  let open_error e = Rresult.R.open_error_exn_trap (Result.error e)

  let trap_exn f v =
    Rresult.R.trap_exn f v
    (* log it now at debug level, just in case it gets lost later *)
    |> on_error ~level:Logs.Debug ~use:open_error ~pp:Rresult.R.pp_exn_trap

  let trap_exn2 f v1 v2 = trap_exn (fun () -> f v1 v2) ()

  module Config = struct
    include Svc.Config

    let of_dict = trap_exn of_dict
  end

  let name = Svc.name

  let validate = trap_exn2 Svc.validate

  let start = trap_exn2 Svc.start

  let reload = trap_exn2 Svc.reload

  let stop = trap_exn2 Svc.stop

  let is_running id ~check_health =
    () |> trap_exn @@ fun () -> Svc.is_running id ~check_health
end
