(** Types *)
module F = Config_field

let rpc_equal typ_of a b =
  (* Rpc is built out of standard types, so stdlib equality works on it  *)
  Rpcmarshal.marshal typ_of a = Rpcmarshal.marshal typ_of b

module StringMap = Map.Make (String)

(** stable representation for serializing/deserializing between XAPI version
   upgrades *)
module V1 = struct
  module Global = struct
    type t = {
        heartbeat_interval: F.milliseconds
      ; election_timeout: F.milliseconds
      ; initial_cluster_token: string
    }
    [@@deriving rpcty]
  end

  module Live = struct
    type t = {
        advertise_client_urls: F.uri list
      ; initial_advertise_peer_urls: F.uri list
      ; initial_cluster: F.uri_map
    }
    [@@deriving rpcty]
  end

  module Local = struct
    type listen_cert = {cert_file: F.path; key_file: F.path} [@@deriving rpcty]

    type tls_listen = AutoTLS | Certs of listen_cert [@@deriving rpcty]

    type tls_auth = {
        trusted_ca_file: F.path
      ; cert_allowed_hostname: string option
      ; cert_allowed_cn: string option
      ; crl_file: F.path option
    }
    [@@deriving rpcty]

    type listen = (tls_listen * tls_auth option) option * F.uri list
    [@@deriving rpcty]

    type config = {
        name: string
      ; initial_cluster_state: F.initial_cluster_state
      ; proxy: bool
      ; quota_backend_bytes: F.positive option
      ; snapshot_count: F.positive option
      ; max_wals: F.limit option
      ; data_dir: F.path option
      ; wal_dir: F.path option
      ; strict_reconfig_check: bool
      ; enable_v2: bool
      ; enable_pprof: bool
      ; self_signed_cert_validity: F.positive option
      ; debug: bool
      ; log_level: F.level option
      ; log_output: F.log_output option
      ; force_new_cluster: bool
      ; cipher_suites: string list
    }
    [@@deriving rpcty]

    type t = {config: config; peer: listen; client: listen} [@@deriving rpcty]
  end
end

type t = V1 of V1.Global.t * V1.Live.t * V1.Local.t [@@deriving rpcty]

(** key-value representation matching etcd.

  Nesting will be flattened, but can only contain record or primitive types
  Field names match [etcd] names but without the [ETCD_] prefix.
*)

type main = t

module Global = struct
  open V1.Global

  type nonrec t = t [@@deriving rpcty]

  let equal = rpc_equal typ_of

  (** defaults may change between etcd versions, use explicit defaults on OCaml
   side instead to ensure consistency on upgrades
   @see https://etcd.io/docs/v3.5/tuning/#time-parameters
  *)
  let make ?(heartbeat_interval = 0.1) ?(election_timeout = 1.0)
      ~initial_cluster_token () =
    let open F in
    if election_timeout < 10. /. 1.5 *. heartbeat_interval then
      Fmt.error_msg
        "Election_timeout (%gs) must be >= 10*heartbeat_interval (%g)"
        election_timeout heartbeat_interval
    else if election_timeout >= 50. then
      Fmt.error_msg "Election_timeout (%g) must be < 50s" election_timeout
    else
      let+ heartbeat_interval = milliseconds heartbeat_interval in
      let+ election_timeout = milliseconds election_timeout in
      Ok {heartbeat_interval; election_timeout; initial_cluster_token}

  let dump = Serialization.dump typ_of
end

module Live = struct
  include V1.Live
  module StringMap = Map.Make (String)

  let empty : t =
    {
      advertise_client_urls= []
    ; initial_advertise_peer_urls= []
    ; initial_cluster= StringMap.empty
    }

  type diff = {
        advertise_client_urls: F.uri list
      ; initial_advertise_peer_urls: F.uri list
      ; initial_cluster: F.uri option StringMap.t
  }

  let list_is_empty = function [] -> true | _ -> false

  let map2 f g (a:t) (b:t) : t =
    {
      advertise_client_urls= f a.advertise_client_urls b.advertise_client_urls
    ; initial_advertise_peer_urls=
        f a.initial_advertise_peer_urls b.initial_advertise_peer_urls
    ; initial_cluster= g a.initial_cluster b.initial_cluster
    }

  let diff_field is_empty next current = if is_empty next then current else next

  let diff_map _ next current =
    match next, current with
    | Some _ as add, None -> Some add
    | None, Some _ -> Some None (* remove *)
    | Some _, Some _ -> None (* this will be a peer URL update *)
    | None, None -> None

  let diff ~next ~current : diff =
    let t = map2 (diff_field list_is_empty) (diff_field StringMap.is_empty) next current in
    { advertise_client_urls = t.advertise_client_urls
    ; initial_advertise_peer_urls = t.initial_advertise_peer_urls
    ; initial_cluster = StringMap.merge diff_map next.initial_cluster current.initial_cluster
    }

  let apply_field is_empty current delta =
    if is_empty delta then current else delta

  let apply_map _ current delta =
    match current, delta with
    | current, None ->  current (* no change *)
    | _, Some None -> None (* remove *)
    | None, Some add ->  add (* add *)
    | Some _, Some modified -> modified

  let apply ~(current : t) (delta:diff) : t =
    { advertise_client_urls = apply_field list_is_empty
    current.advertise_client_urls delta.advertise_client_urls
    ; initial_advertise_peer_urls = apply_field list_is_empty
    current.initial_advertise_peer_urls delta.initial_advertise_peer_urls
    ; initial_cluster = StringMap.merge apply_map current.initial_cluster delta.initial_cluster
    }

  let equal = rpc_equal typ_of
end

module Local = struct
  include V1.Local

  let equal = rpc_equal typ_of
end

let of_main = function V1 (g, live, loc) -> (g, live, loc)

(** Live reload *)

let diff ~next ~current =
  let ng, nlive, nloc = of_main next and cg, clive, cloc = of_main current in
  if not (Global.equal ng cg) then
    Fmt.error_msg
      "Global time configuration cannot be changed on a running\n\
      \    cluster. current:%a next: %a" Global.dump cg Global.dump ng
  else
    Ok
      ( Live.diff ~next:nlive ~current:clive
      , if Local.equal nloc cloc then
          None
        else
          Some nloc
      )

(** Configuration building *)

(** Building *)
module Dict = struct
  module Local = struct
    open V1.Local

    let unpack_tls = function
      | AutoTLS ->
          (None, None, true)
      | Certs t ->
          (Some t.cert_file, Some t.key_file, false)

    let unpack_auth = function
      | None ->
          (false, None, None, None, None)
      | Some t ->
          ( true
          , Some t.trusted_ca_file
          , t.cert_allowed_cn
          , t.cert_allowed_hostname
          , t.crl_file
          )

    let unpack (tls, auth) = (unpack_tls tls, unpack_auth auth)

    let apply_uri_scheme f (tls_opt, uris) =
      let scheme = match tls_opt with None -> "http" | Some _ -> "https" in
      let tls_config = tls_opt |> Option.map unpack |> Option.map f in
      let uris =
        uris |> List.map @@ fun uri -> Uri.with_scheme uri (Some scheme)
      in
      (tls_config, uris)

    type listen_peer_tls = {
        peer_cert_file: F.path option
      ; peer_key_file: F.path option
      ; peer_auto_tls: bool
      ; peer_client_cert_auth: bool
      ; peer_trusted_ca_file: F.path option
      ; peer_cert_allowed_cn: string option
      ; peer_cert_allowed_hostname: string option
      ; peer_crl_file: F.path option
    }
    [@@deriving rpcty]

    let listen_peer_tls
        ( (peer_cert_file, peer_key_file, peer_auto_tls)
        , ( peer_client_cert_auth
          , peer_trusted_ca_file
          , peer_cert_allowed_cn
          , peer_cert_allowed_hostname
          , peer_crl_file
          )
        ) =
      {
        peer_cert_file
      ; peer_key_file
      ; peer_auto_tls
      ; peer_client_cert_auth
      ; peer_trusted_ca_file
      ; peer_cert_allowed_cn
      ; peer_cert_allowed_hostname
      ; peer_crl_file
      }

    type listen_peer = {
        listen_peer_urls: F.uri list
      ; listen_tls: listen_peer_tls option
    }
    [@@deriving rpcty]

    let listen_peer listen =
      let listen_tls, listen_peer_urls =
        apply_uri_scheme listen_peer_tls listen
      in
      {listen_peer_urls; listen_tls}

    type listen_client_tls = {
        cert_file: F.path option
      ; key_file: F.path option
      ; auto_tls: bool
      ; client_cert_auth: bool
      ; trusted_ca_file: F.path option
      ; client_cert_allowed_hostname: string option
      ; client_crl_file: F.path option
    }
    [@@deriving rpcty]

    let listen_client_tls
        ( (cert_file, key_file, auto_tls)
        , ( client_cert_auth
          , trusted_ca_file
          , _
          , client_cert_allowed_hostname
          , client_crl_file
          )
        ) =
      {
        cert_file
      ; key_file
      ; auto_tls
      ; client_cert_auth
      ; trusted_ca_file
      ; client_cert_allowed_hostname
      ; client_crl_file
      }

    type listen_client = {
        listen_client_urls: F.uri list
      ; listen_tls: listen_client_tls option
    }
    [@@deriving rpcty]

    let listen_client listen =
      let listen_tls, listen_client_urls =
        apply_uri_scheme listen_client_tls listen
      in
      {listen_client_urls; listen_tls}

    type t = {config: config; peer: listen_peer; client: listen_client}
    [@@deriving rpcty]

    let make (v1 : V1.Local.t) : t =
      {
        config= v1.config
      ; peer= listen_peer v1.peer
      ; client= listen_client v1.client
      }
  end

  type t = Global.t * Live.t * Local.t [@@deriving rpcty]

  (** Conversion to Environment *)

  let to_env_key_char = function
    | '-' ->
        '_'
    | c ->
        let u = Char.uppercase_ascii c in
        if u <> c then
          invalid_arg "expected ASCII character in config field name" ;
        u

  let to_env_key k = "ETCD_" ^ (k |> String.map to_env_key_char)

  let rec to_environment = function
    | Rpc.Int i64 ->
        Int64.to_string i64
    | Rpc.Int32 i32 ->
        Int32.to_string i32
    | Rpc.Bool b ->
        if b then "true" else "false"
    | Rpc.Float f ->
        Printf.sprintf "%.16g" f
    | Rpc.String s ->
        s
    | Rpc.DateTime s ->
        s
    | Rpc.Enum lst ->
        lst |> List.map to_environment |> String.concat ","
    | Rpc.Dict d ->
        d
        |> List.map (fun (k, v) -> String.concat "" [k; "="; to_environment v])
        |> String.concat ","
    | Rpc.Base64 b64 ->
        Base64.decode_exn b64
    | Rpc.Null ->
        ""

  let to_dict t =
    t |> Rpcmarshal.marshal typ_of |> function
    | Rpc.Dict d ->
        d
        |> List.to_seq
        |> Seq.map (fun (k, v) -> (to_env_key k, to_environment v))
        |> StringMap.of_seq
    | _ ->
        assert false

  let make : main -> t = function
    | V1 (global, live, local) ->
        (global, live, Local.make local)
end

let name t =
  let _, _, local = t |> Dict.make in
  local.Dict.Local.config.name

let make global live local = V1 (global, live, local)

(** Configuration pretty printing *)

let dump = Serialization.dump typ_of

(** Serialization *)

let serialize t = t |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string

let deserialize s =
  s
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal typ_of
  |> Rresult.R.open_error_msg

let to_dict t = t |> Dict.make |> Dict.to_dict
