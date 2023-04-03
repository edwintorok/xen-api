open Etcd_service_types

(** [Make(KVBackend)] creates a KV implementation that uses JSON for
    serialization and talks HTTP/1.1. *)
module Make(_ : KVBackend) : S
