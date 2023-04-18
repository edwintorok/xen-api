(** Implements a subset of the ETCD 3.5 KV service: no transactions or event history.

  @see https://etcd.io/docs/v3.5/dev-guide/api_grpc_gateway/#put-and-get-keys
  @see https://etcd.io/docs/v3.5/dev-guide/api_reference_v3/#service-kv-apietcdserverpbrpcproto
 *)

open Etcd_rpc_types

type status = Status.t

(** Etcd KV proto implementation, the function names here match the 'service
    KV' definitions in the .proto file.

    The [t] type for the backend connection is a convenient place to store file
    descriptors, caches, and other state.
    This makes the interface testable.
*)
module type KVBackend = sig
  type t (** key value backend connection *)

  val name: string
  (** the backend's name *)

  val init: unit -> t
  (** [init ())] initializes a connection a key-value backend.
      Any state needed for the connection can be stored in the return value,
      instead of using global values.
   *)

  val cleanup: t -> unit Lwt.t
  (** [cleanup conn] releases any resources used by [conn].
      Using [conn] after [cleanup] has been called is a programming error.
   *)

  val range : t -> range_request -> (range_response, status) Lwt_result.t
  (** [range conn range_request] gets the keys in the range from the key-value store *)

  val put : t -> put_request -> (put_response, status) Lwt_result.t
  (** [put conn put_request] puts the given key into the key-value store.
      Increments the revision of the key-value store.
   *)

  val delete_range :
    t -> delete_range_request -> (delete_range_response, status) Lwt_result.t
  (** [delete_range conn delete_range_request] deletes the given range from the
      key-value store.
      A delete request increments the revision of the key-value store.
    *)
end

module type S = sig
  type t

  val listen : Unix.sockaddr -> t Lwt.t
  (** [listen addr] listens on the specified [addr]. *)

  val shutdown : t -> unit Lwt.t
  (** [shutdown t] shuts down the listening socket in [t].
      It returns when the shutdown has completed
   *)
end
