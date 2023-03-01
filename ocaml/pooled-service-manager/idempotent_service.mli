module Make (Svc : Service.S) : Service.S with type Config.t = Svc.Config.t
