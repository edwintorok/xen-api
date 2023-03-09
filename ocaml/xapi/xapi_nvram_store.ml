(* open Xapi_etcd

module D = Debug.Make (struct let name = __MODULE__ end)

open D *)

let create ~__context ~network =
  ignore __context; ignore network;
  (*
  info "%s: network = %s" __FUNCTION__ (Ref.string_of network) ;
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in

  let client_url =
    Config.make_uri ~https:false Ipaddr.(V4 V4.localhost) ~port:2379
  in
  let pool = Helpers.get_pool ~__context in
  let cluster_token = "etcd-" ^ Db.Pool.get_uuid ~__context ~self:pool in
  let config =
    Config.default
    |> Config.(add enable_v2 false)
    |> Config.(add initial_cluster_token cluster_token)
    |> Config.(add strict_reconfig_check true)
    |> Config.to_dict
  in

  Db.NVRAM_store.create ~__context ~network ~ref ~uuid ~allowed_operations:[]
    ~current_operations:[] ~backend:`etcd ~client_url:(Uri.to_string client_url)
    ~config ~other_config:[] ;
  ref
  *)
  failwith "TODO"

let destroy ~__context ~self =
  ignore __context; ignore self;
  failwith "TODO"
  (*
  info "%s, self=%s" __FUNCTION__ (Ref.string_of self) ;
  Db.NVRAM_store.destroy ~__context ~self *)
