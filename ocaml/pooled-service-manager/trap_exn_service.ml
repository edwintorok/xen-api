module Make(Svc: Service.S) = struct
  type error = [Rresult.R.msg | Rresult.R.exn_trap]

  type 'a result = ('a, [`Service of error]) Result.t

  let pp_error ppf = function
    | (`Msg _ as msg) -> Rresult.R.pp_msg ppf msg
    | (`Exn_trap _ as exn_trap) -> Rresult.R.pp_exn_trap ppf exn_trap

  let open_error (x: 'a result) = x

  let error_to_msg = function
    | Error (`Exn_trap _) as r ->
        Rresult.R.error_exn_trap_to_msg r
    | r -> r
end
