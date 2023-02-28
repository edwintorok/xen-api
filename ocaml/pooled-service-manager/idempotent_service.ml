open Service

type 'a trap_result = ('a, Rresult.R.exn_trap) result

type 'a msg_or_trap_result = ('a, [Rresult.R.msg | Rresult.R.exn_trap]) result

let reword_error = Rresult.R.error_exn_trap_to_msg

let (let*) = Result.bind

module Make(Svc: S) = struct
  let src = Logs.Src.create __MODULE__
  include (val Logs.src_log src)

  let trap_exn f id config =
    let result = Rresult.R.trap_exn (fun () -> f id config) () in
    let () =
      result |> Result.iter_error @@ fun exn_trap ->
      (* immediately log any exceptions, we may not get another chance.
         Note that we may in the end recover from this
       *)
      debug (fun m -> m "Caught exception: %a" Rresult.R.pp_exn_trap exn_trap)
    in
    result

  module Config = Svc.Config

  let validate = trap_exn Svc.validate_exn

  let state id ~check_health =
    (* TODO: load config from non-persistent *)

  let start id config =
    let* () = validate id config in
    (* TODO: check state *)
    (* start if not already running, etc etc. *)
end
