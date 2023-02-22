open Xapi_etcd

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let ipaddr_of_pif ~__context ~pIF =
  (* TODO: convert to XAPI error *)
  Xapi_pif_helpers.get_primary_address ~__context ~pif:pIF
  |> Option.map Ipaddr.of_string_exn
  |> Option.value ~default:(Ipaddr.V4 Ipaddr.V4.localhost)
(* FIXME: raise proper exn *)

let unique_name_of ~__context ~host =
  Config.Name (Db.Host.get_uuid ~__context ~self:host)

let initial_of_store ~__context ~nvram_store ~host ~peer_url =
  (unique_name_of ~__context ~host, peer_url) ::
  (Db.NVRAM_store_member.get_records_where ~__context
    ~expr:
      Db_filter_types.(
        Eq (Literal (Ref.string_of nvram_store), Field "nvram_store")
      )
  |> List.map @@ fun (_, member) ->
     ( unique_name_of ~__context ~host:member.API.nVRAM_store_member_host
     , Uri.of_string @@ member.nVRAM_store_member_peer_url
     ))

let https = false

let create ~__context ~nvram_store ~host ~pIF =
  info "%s: host = %s; pif = %s" __FUNCTION__ (Ref.string_of host)
    (Ref.string_of pIF) ;
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  (* TODO: proxy through 443 with another SNI/cert *)
  let peer_url =
    Config.make_uri ~https (ipaddr_of_pif ~__context ~pIF) ~port:2380
  in
  let unique_name = unique_name_of ~__context ~host in
  let client_url =
    Db.NVRAM_store.get_client_url ~__context ~self:nvram_store |> Uri.of_string
  in
  let advertise_client_url =
    Uri.with_port peer_url (Some 2379) in
  let initial_cluster_list =
    initial_of_store ~__context ~nvram_store ~host ~peer_url
  in
  (* TODO: config and peer_url should be more dynamic, and should forward it
     through stunnel *)
  let config =
    Config.default
    |> Config.(add log_output [Stderr])
    |> Config.(add name unique_name)
    |> Config.(add initial_cluster initial_cluster_list)
    |> Config.(add listen_peer_urls [peer_url])
    |> Config.(add initial_advertise_peer_urls [peer_url])
    |> Config.(add listen_client_urls [client_url])
    |> Config.(add advertise_client_urls [advertise_client_url])
    (* TODO: peer-transport-security *)
    |> Config.to_dict
  in
  Db.NVRAM_store_member.create ~__context ~ref ~uuid ~allowed_operations:[]
    ~config ~current_operations:[] ~other_config:[]
    ~peer_url:(Uri.to_string peer_url) ~nvram_store ~host ~pIF ~joined:false ;
  ref

let destroy ~__context ~self =
  info "%s: self=%s" __FUNCTION__ (Ref.string_of self) ;
  Db.NVRAM_store_member.destroy ~__context ~self

(* TODO: xapi.conf/resources *)
let etcd_conf = "/etc/etcd/etcd.conf"

let set_joined ~__context ~self ~joined =
  info "%s: self=%s, joined=%b" __FUNCTION__ (Ref.string_of self) joined ;
  (* call etcdctl member add/remove, write out config, start/stop etcd *)
  (* compare current with next config, ask Config for the update list and
     execute actions *)
  let config =
    Db.NVRAM_store_member.get_config ~__context ~self
    |> Config.of_dict
  in
  let str = config |> Config.to_environment_file in
  debug "Writing configuration to %s: %s" etcd_conf str;
  Xapi_stdext_unix.Unixext.write_string_to_file etcd_conf str;
  Xapi_systemctl.restart ~wait_until_success:true "etcd";
  ()
