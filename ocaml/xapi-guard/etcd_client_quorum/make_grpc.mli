open Etcd_service_types

(** [Make(KVBackend)] creates a KV implementation that uses the Protobuf
    protocol for serialization and talks gRPC (which implies HTTP/2). *)
module Make (_ : KVBackend) : S
