(** Implements a subset of the ETCD 3.5 KV service: no transactions or event history.

  @see https://etcd.io/docs/v3.5/dev-guide/api_grpc_gateway/#put-and-get-keys
  @see https://etcd.io/docs/v3.5/dev-guide/api_reference_v3/#service-kv-apietcdserverpbrpcproto
 *)

open Etcd_rpc_types

(** Etcd KV proto implementation, the function names here match the 'service
    KV' definitions in the .proto file *)
module type KVBackend = sig
  val range: range_request -> range_response Lwt.t
  (** [range range_request] gets the keys in the range from the key-value store *)

  val put: put_request -> put_response Lwt.t
  (** [put put_request] puts the given key into the key-value store.
      Increments the revision of the key-value store.
   *)

  val delete_range: delete_range_request -> delete_range_response Lwt.t
  (** [delete_range delete_range_request] deletes the given range from the
      key-value store.
      A delete request increments the revision of the key-value store.
    *)
end

module type S = sig
  type t

  val listen: Unix.sockaddr -> t Lwt.t
  (** [listen addr] listens on the specified [addr]. *)

  val shutdown: t -> unit Lwt.t
  (** [shutdown t] shuts down the listening socket in [t].
      It returns when the shutdown has completed
   *)
end
