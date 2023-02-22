type command = string list

module FieldKind = struct
  type t =
    | Bootstrap
    | Member_command of command
    | Member_startup
    | Remove_first
    | Fixed

  let pp_dump ppf = function
    | Bootstrap ->
        Fmt.string ppf "Bootstrap"
    | Member_command cmd ->
        Fmt.pf ppf "Member_command(%a)" Fmt.Dump.(list string) cmd
    | Member_startup ->
        Fmt.string ppf "Member_startup"
    | Remove_first ->
        Fmt.string ppf "Remove_first"
    | Fixed ->
        Fmt.string ppf "Fixed"
end

module Field = struct
  type t = {key: string; kind: FieldKind.t}

  let compare a b = String.compare a.key b.key

  let pp_dump =
    Fmt.(
      Dump.(
        record
          [
            field "key" (fun t -> t.key) string
          ; field "kind" (fun t -> t.kind) FieldKind.pp_dump
          ]
      )
    )
end

module FieldMap = Map.Make (Field)

type 'a field = Field.t * ('a -> string)

type t = string FieldMap.t

let to_dict t =
  List.of_seq (t |> FieldMap.to_seq |> Seq.map @@ fun (k, v) -> (k.Field.key, v))

let pp_dump =
  Fmt.(Dump.(iter_bindings FieldMap.iter (any "config") Field.pp_dump string))

let to_env_key_char = function
  (* Latest etcd defines only the yaml names,
     and the rule on how the env vars are constructed.
     However systemd unit wants a config file with env vars, not yaml.
  *)
  | '-' ->
      '_'
  | c ->
      Char.uppercase_ascii c

let to_env_key k = "ETCD_" ^ (k |> String.map to_env_key_char)

let string (s : string) = s

let kv_string ?(sep = ",") key value kvlist =
  kvlist
  |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" (key k) (value v))
  |> String.concat sep
  |> string

let to_environment_file t =
  (* Latest etcd defines only the yaml names,
     and the rule on how the env vars are constructed.
     However systemd unit wants a config file with env vars, not yaml.
  *)
  t |> to_dict |> kv_string ~sep:"\n" to_env_key Filename.quote

module Update = struct
  (** Configuration update actions *)

  (** 'etcdctl member' arguments *)
  type nonrec command = command

  (** configuration update actions *)
  type nonrec t = {
      bootstrap: t
          (** initial configuration only relevant during cluster bootstrap, can be subsequently ignored *)
    ; member_commands: command list
          (** configuration that can be changed through 'etcdctl member' *)
    ; member_startup: t
          (** configuration that takes effect after restarting a member *)
    ; remove_first: t
          (** have to remove and readd the member for the change to take effect *)
    ; fixed: t
          (** no documented way of changing. Entire cluster shutdown and start needed. *)
  }

  let pp_dump =
    Fmt.Dump.(
      record
        [
          field "bootstrap" (fun t -> t.bootstrap) pp_dump
        ; field "member_commands"
            (fun t -> t.member_commands)
            (list @@ list string)
        ; field "remove_first" (fun t -> t.remove_first) pp_dump
        ; field "fixed" (fun t -> t.fixed) pp_dump
        ]
    )

  let compute_action _ from target =
    let from = Option.value ~default:"" from
    and target = Option.value ~default:"" target in
    if String.equal from target then
      None (* no change *)
    else
      Some target

  let filter kind = FieldMap.filter (fun field _ -> field.Field.kind = kind)

  let diff ~from ~target =
    let actions = FieldMap.merge compute_action from target in
    let member_commands =
      actions
      |> FieldMap.filter_map @@ fun field target ->
         match field.Field.kind with
         | Member_command command ->
             Some (command @ [target])
         | _ ->
             None
    in
    {
      bootstrap= filter Bootstrap actions
    ; member_commands= member_commands |> FieldMap.bindings |> List.map snd
    ; member_startup= filter Member_startup actions
    ; remove_first= filter Remove_first actions
    ; fixed= filter Fixed actions
    }
end

let diff = Update.diff

type name = Name : string -> name

let int = string_of_int

let int64 = Int64.to_string

let bool = string_of_bool

let path p = p |> Fpath.to_string |> string

let milliseconds seconds = seconds *. 1000. |> int_of_float |> int

let url_list lst =
  (* not a list at the Yaml level, but a comma-separated string *)
  lst |> List.map Uri.to_string |> String.concat "," |> string

let int_or_unlimited = function
  | None ->
      int 0
  | Some 0 ->
      invalid_arg "0 means unlimited, use None instead if you meant unlimited"
  | Some i ->
      int i

module StringMap = Map.Make (String)

let field kind key value_to_string : _ field =
  (Field.{key; kind}, value_to_string)

let proxy = field FieldKind.Remove_first "proxy" string

let add (field, value_to_string) v t = FieldMap.add field (value_to_string v) t

(** [default] is the default configuration that can be extended using the functions in this module. *)
let default = FieldMap.empty |> add proxy "off"

(* v2 only *)

let name_yaml (Name n) = string n

(** [name str t] Human-readable name for this member.*)
let name = field Remove_first "name" name_yaml

(** [data_dir dir t] Path to the data directory.*)
let data_dir = field Member_startup "data-dir" path

(** [wal_dir dir t] to the dedicated wal directory.*)
let wal_dir = field Member_startup "wal-dir" path

(** [snapshot_count n t] Number of committed transactions to trigger a snapshot to disk.*)
let snapshot_count = field Fixed "snapshot-count" int

(** [heartbeat_interval ms t] Time (in milliseconds) of a heartbeat interval.*)
let heartbeat_interval = field Fixed "heartbeat-interval" milliseconds

(** [election_timeout ms t] Time (in milliseconds) for an election to timeout.*)
let election_timeout = field Fixed "election-timeout" milliseconds

(** [quota_backend_bytes quota t] Raise alarms when backend size exceeds the given quota. 0 means use the default quota.*)
let quota_backend_bytes = field Member_startup "quota-backend-bytes" int64

let make_uri ~https ip ~port =
  Uri.make
    ~scheme:(if https then "https" else "http")
    ~host:(Ipaddr.to_string ip) ~port ()

(* https://etcd.io/docs/v3.5/op-guide/runtime-configuration/#update-advertise-peer-urls *)

(** [listen_peer_urls urls t] List of comma separated URLs to listen on for peer traffic.*)
let listen_peer_urls =
  field (Member_command ["update"; "--peer-urls"]) "listen-peer-urls" url_list

(** [listen_client_urls urls t] List of comma separated URLs to listen on for client traffic.*)
let listen_client_urls = field Member_startup "listen-client-urls" url_list

(** [max_snapshots n t] Maximum number of snapshot files to retain (0 is unlimited).*)
let max_snapshots = field Fixed "max-snapshots" int_or_unlimited

(** [max_wals n t] Maximum number of wal files to retain (0 is unlimited).*)
let max_wals = field Fixed "max-wals" int_or_unlimited

(** [advertise_peer_urls urls t] of this member's peer URLs to advertise to the rest of the cluster. *)
let initial_advertise_peer_urls =
  field Member_startup "initial-advertise-peer-urls" url_list

(* see https://etcd.io/docs/v3.5/op-guide/runtime-configuration/#update-advertise-client-urls *)

(** [advertise_client_urls urls t] List of this member's client URLs to advertise to the public.*)
let advertise_client_urls =
  field Member_startup "advertise-client-urls" url_list

let string_of_name (Name n) = n

(** [initial_cluster config t] Initial cluster configuration for bootstrapping.*)
let initial_cluster =
  field Bootstrap "initial-cluster" @@ kv_string string_of_name Uri.to_string

(** [initial_cluster_token secret t] Initial cluster token for the etcd cluster during bootstrap.*)
let initial_cluster_token = field Bootstrap "initial-cluster-token" string

type initial_cluster_state = New | Existing

let initial_cluster_state_yaml = function
  | New ->
      string "new"
  | Existing ->
      string "existing"

(** [initial_cluster_state state t] Initial cluster state ([New] or [Existing]).*)
let initial_cluster_state =
  field Bootstrap "initial-cluster-state" initial_cluster_state_yaml

(** [strict_reconfig_check strict t] Reject reconfiguration requests that would cause quorum loss.*)
let strict_reconfig_check = field Fixed "strict-reconfig-check" bool

(** [enable_v2 enable t] Accept etcd V2 client requests*)
let enable_v2 = field Member_startup "enable-v2" bool

(** [enable_pprof enable t] Enable runtime profiling data via HTTP server*)
let enable_pprof = field Member_startup "enable-pprof" bool

type transport_security = prefix:string -> FieldKind.t -> t -> t

(** [transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file ~auto_tls]

  @param cert_file Path to the client server TLS cert file.
  @param key_file Path to the client server TLS key file.
  @param client_cert_auth Enable client cert authentication.
  @param trusted_ca_file Path to the client server TLS trusted CA key file.
  @param auto-tls Client TLS using generated certificates

*)
let transport_security ~cert_file ~key_file ~client_cert_auth ~trusted_ca_file
    ~auto_tls ~prefix kind t =
  let add_field unprefixed valuetype value =
    add (field kind (prefix ^ "-" ^ unprefixed) valuetype) value
  in
  t
  |> add_field "cert_file" path cert_file
  |> add_field "key-file" path key_file
  |> add_field "client-cert-auth" bool client_cert_auth
  |> add_field "trusted-ca-file" path trusted_ca_file
  |> add_field "auto-tls" bool auto_tls

(** [client_transport_security transport_security t] *)
let client_transport_security ts = ts ~prefix:"client" FieldKind.Member_startup

(** [peer_transport_security transport_security t] *)
let peer_transport_security ts = ts ~prefix:"peer" FieldKind.Fixed

(** [debug enable t] Enable debug-level logging for etcd.*)
let debug = field Member_startup "debug" bool

type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

let string_of_level = function
  | CRITICAL ->
      "CRITICAL"
  | ERROR ->
      "ERROR"
  | WARNING ->
      "WARNING"
  | INFO ->
      "INFO"
  | DEBUG ->
      "DEBUG"

(** [log_package_levels package_levels t] Specify a particular log level for each etcd package.

    @param levels e.g. [etcmain, CRITICAL; etcdserver, DEBUG]
*)
let log_package_levels =
  field Member_startup "log-package-levels" @@ kv_string Fun.id string_of_level

type log_target = Stdout | Stderr | Default

let string_of_log_target = function
  | Stdout -> "stdout"
  | Stderr -> "stderr"
  | Default -> "default"

let log_output =
  field Member_startup "log-output" @@ fun lst ->
  lst |> List.map string_of_log_target |> String.concat ","

(** [force_new_cluster enable t] Force to create a new one member cluster.*)
let force_new_cluster = field Bootstrap "force-new-cluster" bool

let known_fields =
  [
    fst name
  ; fst snapshot_count
  ; fst max_snapshots
  ; fst quota_backend_bytes
  ; fst max_wals
  ; fst wal_dir
  ; fst data_dir
  ; fst heartbeat_interval
  ; fst election_timeout
  ; fst initial_cluster
  ; fst initial_cluster_token
  ; fst initial_cluster_state
  ; fst force_new_cluster
  ; fst listen_peer_urls
  ; fst initial_advertise_peer_urls
  ; fst listen_client_urls
  ; fst advertise_client_urls
  ; fst strict_reconfig_check
  ; fst enable_v2
  ; fst enable_pprof
    (* client_transport_security; and peer_transport_security are not overridable
       this way *)
  ; fst debug
  ; fst log_package_levels
  ]
  |> List.to_seq
  |> Seq.map (fun field -> (field.Field.key, field))
  |> StringMap.of_seq

let other key =
  match StringMap.find_opt key known_fields with
  | Some found ->
      (found, string)
  | None ->
      field Fixed key string

let of_dict dict =
  dict |> List.fold_left (fun acc (k, v) -> add (other k) v acc) default
