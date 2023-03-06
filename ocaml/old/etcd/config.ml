type command = string * string list

type live = command list
type global = unit
type local = unit

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let field_set fields =
  fields |> List.to_seq |> StringSet.of_seq

let transport_security kind =
  let k f = Config_field.key (f ~kind) in
  Config_field.Transport_security.
  [ k cert_file
  ; k key_file
  ; k trusted_ca_file
  ; k client_cert_auth
  ; k cert_allowed_cn
  ; k cert_allowed_hostname
  ; k crl_file
  ; k auto_tls
  ]

let global_fields =
  let k = Config_field.key in
  Config_field.
  [ k snapshot_count
  ; k max_snapshots
  ; k quota_backend_bytes
  ; k heartbeat_interval
  ; k election_timeout
  ; k initial_cluster_token
  ; k strict_reconfig_check
  ; k cipher_suites
  ] @ (transport_security Peer) (* TODO: how about PSR/certificate rotation? *)
  |> field_set

let local_fields =
  let k = Config_field.key in
  Config_field.
  [ k proxy
  ; k name
  ; k max_wals
  ; k wal_dir
  ; k data_dir
  ; k listen_peer_urls
  ; k initial_cluster_state
  ; k force_new_cluster (* TODO: remove? *)
  ; k listen_client_urls
  ; k enable_v2
  ; k enable_pprof
  ; k debug
  ; k log_package_levels
  ; k log_output
  ; k self_signed_cert_validity
  ] @ (transport_security Client) |> field_set

let live_fields =
  let k = Config_field.key in
  Config_field.
  (* TODO: should be a function that given old/new determines commmands to run *)
  [ k initial_cluster, []
  ; k initial_advertise_peer_urls, []
  ; k advertise_client_urls, []
  ] |> List.to_seq |> StringMap.of_seq

module Section = struct
  type 'a t = command list * Config_file.t

  type 'a kind = Global: global kind | Local: local kind | Live: live kind

  let global = Global
  let live = Live
  let local = Local

  let union_exn (live1, config1) (live2, config2) =
    (live1 @ live2), Config_file.union_exn config1 config2

  let of_field_value_exn (type a) (kind:a kind) field value : _ t =
    let key = Config_field.key field in
    let result =  Config_file.(add_exn field value empty) in
    match kind with
    | Live ->
        (match StringMap.find_opt key live_fields with
        | Some cmds -> cmds, result
        | None ->
            Fmt.invalid_arg "Key %S is not a live configuration field" key)
    | Global ->
        if not (StringSet.mem key global_fields) then
          Fmt.invalid_arg "Key %S is not a global configuration field" key;
        [], result
    | Local ->
        if not (StringSet.mem key local_fields) then
          Fmt.invalid_arg "Key %S is not a local configuration field" key;
        [], result

  let to_dict _ (_, config) = Config_file.to_dict config

  let of_dict kind dict =
    dict |> List.fold_left (fun acc (key, value) ->
      of_field_value_exn kind (Config_field.other key) value
      |> union_exn acc
    ) ([], Config_file.empty) |> Result.ok

  (* TODO: separate delta type from regular section type? *)
  (* let delta ~current:(_, current_config) ~desired:(_, desired_config) =
    Config_file.delta ~current:current_config ~desired:desired_config *)

  let apply_live ((cmds, _):live t) = cmds
end

type t =
{ live: live Section.t (** Live configuration to change, or bootstrap configuration to start new cluster *)
; global: global Section.t (** Configuration that all members in the cluster need to agree on. Usually it cannot be changed live. *)
; local: local Section.t (** Configuration local to a member, that can be changed by simply restarting the member *)
}

let to_environment_file t =
  let config =
    [t.live; t.global; t.local]
    |> List.map snd |> List.map Config_file.to_environment_file
  in
  ("# Automatically generated configuration file. DO NOT EDIT, your changes WILL be overwritten!" :: config)
  |> String.concat "\n\n"
