module Make(Svc: Service.S) : Service.S with type error = [Rresult.R.msg | Rresult.R.exn_trap]
