open Xapi_etcd

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let ipaddr_of_pif ~__context ~pIF =
  (* TODO: convert to XAPI error *)
  Xapi_pif_helpers.get_primary_address ~__context ~pif:pIF
  |> Option.map Ipaddr.of_string_exn
  |> Option.get
(* FIXME: raise proper exn *)

let unique_name_of ~__context ~host =
  Config.Name (Db.Host.get_uuid ~__context ~self:host)

let initial_of_store ~__context ~nvram_store =
  Db.NVRAM_store_member.get_records_where ~__context
    ~expr:
      Db_filter_types.(
        Eq (Literal (Ref.string_of nvram_store), Field "nvram_store")
      )
  |> List.map @@ fun (_, member) ->
     ( unique_name_of ~__context ~host:member.API.nVRAM_store_member_host
     , Uri.of_string @@ member.nVRAM_store_member_peer_url
     )

let create ~__context ~nvram_store ~host ~pIF =
  info "%s: host = %s; pif = %s" __FUNCTION__ (Ref.string_of host)
    (Ref.string_of pIF) ;
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  (* TODO: proxy through 443 with another SNI/cert *)
  let peer_url =
    Config.make_uri ~https:true (ipaddr_of_pif ~__context ~pIF) ~port:2380
  in
  let name = unique_name_of ~__context ~host in
  let client_url =
    Db.NVRAM_store.get_client_url ~__context ~self:nvram_store |> Uri.of_string
  in
  (* TODO: config and peer_url should be more dynamic *)
  let config =
    Config.default
    |> Config.name name
    |> Config.initial_cluster @@ initial_of_store ~__context ~nvram_store
    |> Config.listen_peer_urls [peer_url]
    |> Config.initial_advertise_peer_urls [peer_url]
    |> Config.listen_client_urls [client_url]
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

let set_joined ~__context ~self ~joined =
  info "%s: self=%s, joined=%b" __FUNCTION__ (Ref.string_of self) joined ;
  (* call etcdctl member add/remove, write out config, start/stop etcd *)
  ()
