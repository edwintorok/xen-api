open Types
module Make (Svc : Service) : Service with type Config.t = Svc.Config.t
