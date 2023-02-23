type 'a t = {key: string; value_to_string_exn: 'a -> string}

let to_env_key_char = function
  | '-' ->
      '_'
  | c ->
      let u = Char.uppercase_ascii c in
      if u <> c then invalid_arg "expected ASCII character in config field name" ;
      u

let to_env_key k = "ETCD_" ^ (k |> String.map to_env_key_char)

exception FieldError of (string * exn)

let () =
  Printexc.register_printer @@ function
  | FieldError (field, e) ->
      Some
        (Printf.sprintf "field %S conversion error: %s" field
        @@ Printexc.to_string e
        )
  | _ ->
      None

let to_string_pair_exn field value =
  match (to_env_key field.key, field.value_to_string_exn value) with
  | exception e ->
      raise @@ FieldError (field.key, e)
  | result ->
      result

(* Field value conversions *)

let kv_string ?(sep = ",") key value kvlist =
  kvlist
  |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" (key k) (value v))
  |> String.concat sep

let string (s : string) = s

let positive (i : int) =
  if i < 0 then invalid_arg "argument must be positive" ;
  string_of_int i

let positive_int64 (i : int64) =
  if Int64.(compare i zero) < 0 then invalid_arg "argument must be positive" ;
  Int64.to_string i

let bool = string_of_bool

let milliseconds (seconds : float) =
  seconds *. 1000. |> Float.round |> int_of_float |> positive

(* Field definitions *)

let field key value_to_string_exn = {key; value_to_string_exn}

let other key = field key string

let proxy = field "proxy" bool

type name = Name : string -> name

let name = field "name" @@ fun (Name n) -> n

let path = Fpath.to_string

let list conv lst =
  (* even in Yaml this is just a comma separated string, not a list *)
  lst |> List.map conv |> String.concat "," |> string

let url_list = list Uri.to_string

let int_or_unlimited = function
  | None ->
      positive 0
  | Some 0 ->
      invalid_arg "0 means unlimited, use None instead if you meant unlimited"
  | Some i ->
      positive i

let data_dir = field "data-dir" path

let wal_dir = field "wal-dir" path

let snapshot_count = field "snapshot-count" positive

let heartbeat_interval = field "heartbeat-interval" milliseconds

let election_timeout = field "election-timeout" milliseconds

let quota_backend_bytes = field "quota-backend-bytes" positive_int64

let make_uri ~https ip ~port =
  Uri.make
    ~scheme:(if https then "https" else "http")
    ~host:(Ipaddr.to_string ip) ~port ()

let listen_peer_urls = field "listen-peer-urls" url_list

let listen_client_urls = field "listen-client-urls" url_list

let max_snapshots = field "max-snapshots" int_or_unlimited

let max_wals = field "max-wals" int_or_unlimited

let initial_advertise_peer_urls = field "initial-advertise-peer-urls" url_list

let advertise_client_urls = field "advertise-client-urls" url_list

let string_of_name (Name n) = n

let initial_cluster =
  field "initial-cluster" @@ kv_string string_of_name Uri.to_string

let initial_cluster_token = field "initial-cluster-token" string

type initial_cluster_state = New | Existing

let initial_cluster_state_yaml = function
  | New ->
      string "new"
  | Existing ->
      string "existing"

(** [initial_cluster_state state t] Initial cluster state ([New] or [Existing]).*)
let initial_cluster_state =
  field "initial-cluster-state" initial_cluster_state_yaml

let strict_reconfig_check = field "strict-reconfig-check" bool

let enable_v2 = field "enable-v2" bool

let enable_pprof = field "pprof" bool

module Transport_security = struct
  type kind = Client | Peer

  let prefix_of ~always = function
    | Client ->
        if always then "client-" else ""
    | Peer ->
        "peer-"

  let field ?(always = false) ~kind suffix =
    field (prefix_of ~always kind ^ suffix)

  let cert_file = field "cert-file" path

  let key_file = field "key-file" path

  let trusted_ca_file = field "trusted-ca-file" path

  let client_cert_auth = field "client-cert-auth" bool

  let cert_allowed_cn = field "cert-allowed-cn" string

  (* these 2 fields do not follow conventions, and prefix client config with
     client- *)
  let cert_allowed_hostname = field ~always:true "cert-allowed-hostname" string

  let crl_file = field ~always:true "crl-file" path

  let auto_tls = field "auto-tls" bool
end

let self_signed_cert_validity = field "self-signed-cert-validity" positive

let cipher_suites = field "cipher-suites" @@ list string

let debug = field "debug" bool

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
  field "log-package-levels" @@ kv_string Fun.id string_of_level

type log_target = Stdout | Stderr | Default

let string_of_log_target = function
  | Stdout ->
      "stdout"
  | Stderr ->
      "stderr"
  | Default ->
      "default"

let log_output =
  field "log-output" @@ fun lst ->
  lst |> List.map string_of_log_target |> String.concat ","

let force_new_cluster = field "force-new-cluster" bool
