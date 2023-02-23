(** {1:Configuration building} *)

(** configuration field with values of type ['a] *)
type 'a field

(** etcd configuration *)
type t

val default : t
(** [default] is the default configuration that can be extended using the functions in this module. *)

val add : 'a field -> 'a -> t -> t
(** [add field value config] adds [field=value] to [config]. *)

(** {1:Configuration conversion} *)

val to_environment_file : t -> string
(** [to_environment_file] returns a [key=value] representation of [config] suitable for a systemd EnvironmentFile. *)

val of_dict : (string * string) list -> t
(** [of_dict dict] returns a config {!t} from a list of key-value pairs in [dict]. *)

val to_dict : t -> (string * string) list
(** [to_dict config] returns a list of key-value pairs of [config] *)

(** {1:Configuration fields} *)

(** {2:Member configuration} *)

type name = Name : string -> name  (** etcd member name *)

val name : name field
(** [name] Human-readable name for this member.

  Each member must have a unique name when using discovery.
*)

(** {2:Snapshot configuration} *)

val snapshot_count : int field
(** [snapshot_count] Number of committed transactions to trigger a snapshot to disk. *)

val max_snapshots : int option field
(** [max_snapshots] Maximum number of snapshot files to retain.

  Default: 5.

  @param n snapshots, or [None] for unlimited
*)

val quota_backend_bytes : int64 field
(** [quota_backend_bytes] Raise alarms when backend size exceeds the given quota. 0 means use the default quota.
  0 means the default quota (2GB), recommended not to exceed 8GB.

*)

(** {2:WAL configuration} *)

val max_wals : int option field
(** [max_wals] Maximum number of wal files to retain.

  Default: 5.

  @param n wals, or [None] for unlimited
*)

val wal_dir : Fpath.t field
(** [wal_dir] path to the dedicated WAL directory to optimize performance.

  Default: same as {!data_dir}.
*)

val data_dir : Fpath.t field
(** [data_dir] path to the data directory.

  A suitable default exists, accessible only by etcd daemon.
*)

(** {2:Time configuration} *)

val heartbeat_interval : float field
(** [heartbeat_interval] Time (with millisecond resolution) of a heartbeat interval.
  Best practice: should be [0.5, 1.5] RTT between members, default 100ms.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

val election_timeout : float field
(** [election_timeout] Time (with millisecond resolution) for an election to timeout.

  Best practice: 10*RTT between members.
  Upper limit is 50s.

  @param seconds timeout as a float, converted internally to milliseconds

  @see <https://etcd.io/docs/v3.2/tuning/> tuning guide
*)

(** {2:Network configuration} *)

val make_uri : https:bool -> Ipaddr.t -> port:int -> Uri.t
(** [make_uri ~https ip ~port] constructs a URI suitable for configuration flags in this module. *)

(** {2:Initial static bootstrap configuration} only used during initial bootstrap, but not for restarts *)
val initial_cluster : (name * Uri.t) list field
(** [initial_cluster] Initial cluster configuration for bootstrapping.*)

val initial_cluster_token : string field
(** [initial_cluster_token] Initial cluster token for the etcd cluster during bootstrap.
  Each cluster should have a unique cluster token (including creating/destroy a single cluster)
*)

type initial_cluster_state = New | Existing

val initial_cluster_state : initial_cluster_state field
(** [initial_cluster_state] Initial cluster state ([New] or [Existing]).*)

val force_new_cluster : bool field
(** [force_new_cluster] Force to create a new one member cluster.*)

(** {2:Peer URLs} *)

val listen_peer_urls : Uri.t list field
(** [listen_peer_urls] List of URLs to listen on for peer traffic.

  Default: http://localhost:2380

  @param urls list of URLs containing IP addresses, NOT domain names, with the exception of 'localhost'

*)

val initial_advertise_peer_urls : Uri.t list field
(** [initial_advertise_peer_urls] List of this member's peer URLs to advertise to the rest of the cluster.
  Used during initial (static) bootstrap [initial_advertise_peer_urls] and
  updatable live via [etcdctl member update]
*)

(** {2:Client URLs} *)

val listen_client_urls : Uri.t list field
(** [listen_client_urls] List of URLs to listen on for client traffic.*)

val advertise_client_urls : Uri.t list field
(** [advertise_client_urls] List of this member's client URLs to advertise to the public.

  @see <https://etcd.io/docs/v3.5/op-guide/runtime-configuration/#update-advertise-client-urls>
*)

(** {2:Reconfiguration} *)

val strict_reconfig_check : bool field
(** [strict_reconfig_check] Reject reconfiguration requests that would cause quorum loss.*)

(** {2:Protocol} *)

val enable_v2 : bool field
(** [enable_v2] Accept etcd V2 client requests.

Note that v2 and v3 are 2 entirely different datastores, and migration is only
possible as a one-off in one direction.

*)

(** {2:Tweaks} *)
val enable_pprof : bool field
(** [enable_pprof] Enable runtime profiling data via HTTP server*)

(** {2:TLS configuration} *)

(** etcd transport security configuration *)
type transport_security

val transport_security :
     cert_file:Fpath.t
  -> key_file:Fpath.t
  -> client_cert_auth:bool
  -> trusted_ca_file:Fpath.t
  -> auto_tls:bool
  -> transport_security
(** [transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file ~auto_tls]

  @param cert_file Path to the client server TLS cert file.
  @param key_file Path to the client server TLS key file.
  @param client_cert_auth Enable client cert authentication.
  @param trusted_ca_file Path to the client server TLS trusted CA key file.
  @param auto-tls Client TLS using generated certificates
*)

val client_transport_security : transport_security -> t -> t
(** [client_transport_security ts t] *)

val peer_transport_security : transport_security -> t -> t
(** [peer_transport_security ts t] *)

(** {2:Logging configuration} *)

val debug : bool field
(** [debug] Enable debug-level logging for etcd.*)

type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

val log_package_levels : (string * level) list field
(** [log_package_levels] Specify a particular log level for each etcd package.

    @param levels e.g. [etcmain, CRITICAL; etcdserver, DEBUG]
*)

type log_target =
  | Stdout  (** send log messages to standard output *)
  | Stderr  (** send log messages to standard error *)
  | Default  (** send log messages to journald when running under systemd *)

val log_output : log_target list field
(** [log_output] Specify where to write log output.

  Note: the [Default] target is buggy under our version of systemd+journald:
  when logging to the journal upon startup failure it only logs 1 line,
  and journald furthermore loses that line when it forwards to rsyslog.
  (it only works if StandardError is set to 'kmsg')

  Setting [log_output] to [Stderr] is a lot more reliable, and prevents losing
  log lines.

  @param targets e.g. [Stderr].
*)

(** {2:Other configuration} *)

val other : string -> string field
(** [other key] is a custom [key], with string [value].

  Note that if [key] matches one of the known config types above it will still
  result in the correct update action, however unknown fields will result in a
  [Fixed] action (not changeable without cluster daemon shutdown and startup).
*)

module Update : sig
  (** Configuration update actions *)

  (** 'etcdctl member' arguments *)
  type command = string list

  (** configuration update actions *)
  type nonrec t = {
      bootstrap: t
          (** initial configuration only relevant during cluster bootstrap, can be subsequently ignored *)
    ; member_commands: command list
          (** configuration that can be changed through 'etcdctl member' followed by a (re)start of the member *)
    ; member_startup: t
          (** configuration that takes effect after restarting a member *)
    ; remove_first: t
          (** have to remove and readd the member for the change to take effect *)
    ; fixed: t
          (** no documented way of changing. Entire cluster shutdown and start needed. *)
  }

  val pp_dump : t Fmt.t
  (** [pp_dump ppf t] is a string representation of [t] for debugging. *)
end

val diff : from:t -> target:t -> Update.t
(** [diff ~from ~target] is a definition of the update actions {!t} that need to be performed to get an etcd cluster from state [from] to state [to]. *)
