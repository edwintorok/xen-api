(** etcd configuration entry with value of type ['a] *)
type 'a t

val key: _ t -> string
(** [key field] is the command line flag for [field]. *)

val other : string -> string t
(** [other key] is a custom [key], with string [value]. *)

val to_string_pair_exn : 'a t -> 'a -> string * string

val proxy : bool t
(** [proxy] enables or disables proxy mode. Only works with V2 protocol. *)

type name = Name : string -> name  (** etcd member name *)

val name : name t
(** [name] Human-readable name for this member.

  Each member must have a unique name when using discovery.
*)

val snapshot_count : int t
(** [snapshot_count] Number of committed transactions to trigger a snapshot to disk. *)

val max_snapshots : int option t
(** [max_snapshots] Maximum number of snapshot files to retain.

  Default: 5.

  @param n snapshots, or [None] for unlimited
*)

val quota_backend_bytes : int64 t
(** [quota_backend_bytes] Raise alarms when backend size exceeds the given quota. 0 means use the default quota.
  0 means the default quota (2GB), recommended not to exceed 8GB.

*)

val max_wals : int option t
(** [max_wals] Maximum number of wal files to retain.

  Default: 5.

  @param n wals, or [None] for unlimited
*)

val wal_dir : Fpath.t t
(** [wal_dir] path to the dedicated WAL directory to optimize performance.

  Default: same as {!data_dir}.
*)

val data_dir : Fpath.t t
(** [data_dir] path to the data directory.

  A suitable default exists, accessible only by etcd daemon.
*)

val heartbeat_interval : float t
(** [heartbeat_interval] Time (with millisecond resolution) of a heartbeat interval.
  Best practice: should be [0.5, 1.5] RTT between members, default 100ms.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

val election_timeout : float t
(** [election_timeout] Time (with millisecond resolution) for an election to timeout.

  Best practice: 10*RTT between members.
  Upper limit is 50s.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

val make_uri : https:bool -> Ipaddr.t -> port:int -> Uri.t
(** [make_uri ~https ip ~port] constructs a URI suitable for configuration flags in this module. *)

(** {2:Initial static bootstrap configuration} only used during initial bootstrap, but not for restarts *)
val initial_cluster : (name * Uri.t) list t
(** [initial_cluster] Initial cluster configuration for bootstrapping.*)

val initial_cluster_token : string t
(** [initial_cluster_token] Initial cluster token for the etcd cluster during bootstrap.
  Each cluster should have a unique cluster token (including creating/destroy a single cluster)
*)

type initial_cluster_state = New | Existing

val initial_cluster_state : initial_cluster_state t
(** [initial_cluster_state] Initial cluster state ([New] or [Existing]).*)

val force_new_cluster : bool t
(** [force_new_cluster] Force to create a new one member cluster.*)

val listen_peer_urls : Uri.t list t
(** [listen_peer_urls] List of URLs to listen on for peer traffic.

  Default: http://localhost:2380

  @param urls list of URLs containing IP addresses, NOT domain names, with the exception of 'localhost'

*)

val initial_advertise_peer_urls : Uri.t list t
(** [initial_advertise_peer_urls] List of this member's peer URLs to advertise to the rest of the cluster.
  Used during initial (static) bootstrap [initial_advertise_peer_urls] and
  updatable live via [etcdctl member update]
*)

val listen_client_urls : Uri.t list t
(** [listen_client_urls] List of URLs to listen on for client traffic.*)

val advertise_client_urls : Uri.t list t
(** [advertise_client_urls] List of this member's client URLs to advertise to the public.

  @see <https://etcd.io/docs/v3.5/op-guide/runtime-configuration/#update-advertise-client-urls>
*)

val strict_reconfig_check : bool t
(** [strict_reconfig_check] Reject reconfiguration requests that would cause quorum loss.*)

val enable_v2 : bool t
(** [enable_v2] Accept etcd V2 client requests.

Note that v2 and v3 are 2 entirely different datastores, and migration is only
possible as a one-off in one direction.

*)

val enable_pprof : bool t
(** [enable_pprof] Enable runtime profiling data via HTTP server*)

module Transport_security : sig
  type kind =
    | Client  (** defines the etcd client to server transport security *)
    | Peer  (** defines the etcd server to server transport security *)

  val cert_file : kind:kind -> Fpath.t t
  (** [cert_file] Path to the TLS (public) certificate file *)

  val key_file : kind:kind -> Fpath.t t
  (** [key_file] Path to the TLS private key file *)

  val trusted_ca_file : kind:kind -> Fpath.t t
  (** [trusted_ca_file] Path to trusted CA file for checking certificates *)

  val client_cert_auth : kind:kind -> bool t
  (** [client_cert_auth] enable TLS client certificate authentication *)

  val cert_allowed_cn : kind:kind -> string t
  (** [cert_allowed_cn] required TLS CommonName for client certs *)

  val cert_allowed_hostname : kind:kind -> string t
  (** [cert_allowed_hostname] required TLS CommonName for client certs *)

  val crl_file : kind:kind -> Fpath.t t
  (** [crl_file] Path to certificate revocation list file *)

  val auto_tls : kind:kind -> bool t
  (** [auto_tls] generate and use self-signed certificates if [key_file] and
     [cert_file] are not provided *)
end

val self_signed_cert_validity : int t
(** [self_signed_validity] validity in years for generated certificates when {!Transport_security.auto_tls} is used. *)

val cipher_suites : string list t
(** [cipher_suites] list of supported TLS cipher suites *)

val debug : bool t
(** [debug] Enable debug-level logging for etcd.*)

type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

val log_package_levels : (string * level) list t
(** [log_package_levels] Specify a particular log level for each etcd package.

    @param levels e.g. [etcmain, CRITICAL; etcdserver, DEBUG]
*)

type log_target =
  | Stdout  (** send log messages to standard output *)
  | Stderr  (** send log messages to standard error *)
  | Default  (** send log messages to journald when running under systemd *)

val log_output : log_target list t
(** [log_output] Specify where to write log output.

  Note: the [Default] target is buggy under our version of systemd+journald:
  when logging to the journal upon startup failure it only logs 1 line,
  and journald furthermore loses that line when it forwards to rsyslog.
  (it only works if StandardError is set to 'kmsg')

  Setting [log_output] to [Stderr] is a lot more reliable, and prevents losing
  log lines.

  @param targets e.g. [Stderr].
*)
