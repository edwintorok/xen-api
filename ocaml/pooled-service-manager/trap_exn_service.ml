module Make(Svc: Service.S) = struct
  type error = [Rresult.R.msg | Rresult.R.exn_trap]

  type 'a result = ('a, [`Service of error]) Result.t

  let pp_error ppf = function
    | (`Service (`Msg _ as msg)) -> Rresult.R.pp_msg ppf msg
    | (`Service (`Exn_trap _ as exn_trap)) -> Rresult.R.pp_exn_trap ppf exn_trap

  let open_error = function
    | Ok _ as ok -> ok
    | Error (`Service e) -> Error (`Service e)

  let service_error x = `Service x

  let error_to_msg : 'a result -> ('a, Rresult.R.msg) Result.t  = function
    | Error (`Service (`Exn_trap _ as e)) ->
        Rresult.R.error_exn_trap_to_msg (Error e)
    | Error (`Service (`Msg _ as e)) ->
        Error e
    | Ok _ as ok -> ok

  let of_svc_result r =
    let open Rresult.R in
    r |> Svc.error_to_msg |> open_error_msg |> reword_error service_error

  let trap_exn (f : 'a -> 'b Svc.result ) v : 'b result =
    (* |> map of_svc_result |> reword_error service_error |> join *)
    match Rresult.R.trap_exn f v with
    | Ok no_trap -> of_svc_result no_trap
    | Error trap -> Error (service_error trap)

  let trap_exn2 f v1 v2 =
    trap_exn (fun () -> f v1 v2) ()

  module Config = struct
    include Svc.Config

    let of_dict = trap_exn of_dict
  end

  let name = Svc.name

  let validate = trap_exn2 Svc.validate
  let start = trap_exn2 Svc.start
  let reload = trap_exn2 Svc.reload
  let stop id ~force = trap_exn @@ Svc.stop id ~force
  let is_running id ~check_health =
    () |> trap_exn @@ fun () -> Svc.is_running id ~check_health
end
