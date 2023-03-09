val etcdctl : Fpath.t ref
(** [etcdctl] is the location of the [etcdctl] command. *)

val reload: next:Config.t -> current:Config.t -> (Config.t option, [>`Msg of string]) result
(** [reload] reloads the configuration to move from [current] to [next] if possible.
    This may involve restarting the local node

    @return the local configuration to restart with if any
    *)
