exception Non_master_login_on_slave

val check_local_session_hook :
  (__context:Context.nodb Context.t -> session_id:[`session] Ref.t -> bool) option ref

val is_local_session : [< Context.kind] Context.t -> [`session] Ref.t -> bool

val check :
  intra_pool_only:bool -> session_id:[`session] Ref.t -> action:string -> unit
