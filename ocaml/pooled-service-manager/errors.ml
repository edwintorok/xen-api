type error = [Rresult.R.msg | Rresult.R.exn_trap]

let equal_error (a : [< error]) (b : [< error]) = a = b

let open_error = function
  | Ok _ as r ->
      r
  | Error (`Msg _ as e) ->
      Error e
  | Error (`Exn_trap _ as e) ->
      Error e

let pp_error ppf = function
  | `Msg _ as msg ->
      Rresult.R.pp_msg ppf msg
  | `Exn_trap _ as exn_trap ->
      Rresult.R.pp_exn_trap ppf exn_trap

let error_to_msg = function
  | Error (`Exn_trap _ as e) ->
      Rresult.R.error_exn_trap_to_msg (Error e)
  | Error (`Msg _ as e) ->
      Error e
  | Ok _ as ok ->
      ok
