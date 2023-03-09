(** XAPI metadata storage configuration

  See {{!page-metastore_design.section-configuration} the design}.

*)

open Config_field

(** Cluster membership time parameters cannot be safely changed while the
    cluster is running, and etcd has no built-in support for changing these,
    or ensuring their consistency.

    @see https://etcd.io/docs/v3.5/tuning/#time-parameters
    "Setting different values for etcd members may disrupt cluster stability"
*)
module Global : sig
  (** configuration that cannot be safely changed once the cluster is running. *)
  type t

  val make :
       ?heartbeat_interval:float
    -> ?election_timeout:float
    -> initial_cluster_token:string
    -> unit
    -> (t, _) result
  (** [make ?heartbeat_interval ?election_timeout ~initial_cluster_token ()] are cluster membership
      global time parameters.
      All parameters are in seconds, with millisecond resolution (conversion
      happens internally).

      @param heartbeat_interval frequency of leader notifications (ideally [0.5 RTT; 1.5 RTT]), default 100ms
      @param election_timeout to trigger a leader election on lack of hearbeats, default 1000ms
      @param initial_cluster_token prevents unintended cross-cluster interaction

      @see https://etcd.io/docs/v3.5/tuning/#time-parameters
      *)

  val equal : t -> t -> bool
  (** [equal a b] compares global configurations for equality *)
end

(** Configuration that can be queried and changed via [etcdctl member] commands
    while the cluster is running.
    Also used for bootstrapping a cluster.
 *)
module Live : sig
  type t = {
      advertise_client_urls: uri list
    ; initial_advertise_peer_urls: uri list
    ; initial_cluster: uri_map
  }

  type diff = {
      advertise_client_urls: uri list
    ; initial_advertise_peer_urls: uri list
    ; initial_cluster: uri option Map.Make(String).t
  }

  val empty : t
  (** [empty] is the configuration with all fields at their default values.

    [equal (apply ~current empty) current]
    [equal (diff ~next:current ~current) empty]
   *)

  val diff : next:t -> current:t -> diff
  (** [diff ~next ~current] are the field values that have changed from
      [current] to [next]. Unchanged fields are represented by empty
      lists/maps.

      [equal (apply ~current (diff ~next ~current)) next]
  *)

  val apply : current:t -> diff -> t
  (** [apply ~current delta] applies the [delta] change on top of [current].
      Empty lists/maps mean to use the values from [current], otherwise it
      overrides the values completely.

      [diff ~next:(apply ~current ~delta) ~current = delta]
   *)

  val equal : t -> t -> bool
  (** [equal a b] compares configurations for equality *)
end

(** Configuration local to a member, changed by restarting the member *)
module Local : sig
  type listen_cert = {cert_file: path; key_file: path}

  type tls_listen = AutoTLS | Certs of listen_cert

  type tls_auth = {
      trusted_ca_file: path
    ; cert_allowed_hostname: string option
    ; cert_allowed_cn: string option
    ; crl_file: path option
  }

  type listen = (tls_listen * tls_auth option) option * uri list

  type config = {
      name: string
    ; initial_cluster_state: initial_cluster_state
    ; proxy: bool
    ; quota_backend_bytes: positive option
    ; snapshot_count: positive option
    ; max_wals: limit option
    ; data_dir: path option
    ; wal_dir: path option
    ; strict_reconfig_check: bool
    ; enable_v2: bool
    ; enable_pprof: bool
    ; self_signed_cert_validity: positive option
    ; debug: bool
    ; log_level: level option
    ; log_output: log_output option
    ; force_new_cluster: bool
    ; cipher_suites: string list
  }

  type t = {config: config; peer: listen; client: listen}

  val equal : t -> t -> bool
  (** [equal a b] compares configurations for equality *)
end

(** XAPI metadata storage configuration *)
type t

val name: t -> string
(** [name t] retrieves the name of the local member *)

val make : Global.t -> Live.t -> Local.t -> t
(** [make global live local] constructs a configuration out of
    [globaltime], [live] and [local] configuration.
 *)

val diff : next:t -> current:t -> (Live.diff * Local.t option, _) result
(** [diff ~next ~current] determines how to get from [current] configuration to
    [next].

    This is only possible if none of the {!GlobalTime} values have been
    changed.

    @return the live configuration delta that needs to be applied and the new
    configuration for a member restart if needed
 *)

val dump : t Fmt.t
(** [dump ppf t] prints a representation of [t] on [ppf] for debugging. *)

val to_dict : t -> string Map.Make(String).t
(** [to_dict t] is a string representation of [t], suitable for a
    configuration file for the metadata storage daemon.

    For etcd these will be environment variables.
*)

val serialize : t -> string
(** [serialize t] serializes [t] into a string.
    This can be converted back into its original form using {!of_string}.

    It is not called [to_string], because the representation may be completely
    different to the one used by the daemon's native configuration format.
*)

val deserialize : string -> (t, _) result
(** [deserialize s] deserializes [s] as a configuration.
  It supports loading configuration serialized with previous versions of this
  module too.

  @return [Ok t] if the configuration got deserialized successfully,
  [Error (`Msg reason)] otherwise
*)
