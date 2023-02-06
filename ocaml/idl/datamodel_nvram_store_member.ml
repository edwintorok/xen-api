open Datamodel_common
open Datamodel_roles
open Datamodel_types

(* NVRAM storage for UEFI variables and vTPM *)

(* for allowed operations *)
let nvram_store_member_operation =
  Enum
    ( "nvram_store_member_operation"
    , [
        ( "membership_change"
        , "changing membership of NVRAM storage cluster member"
        )
      ]
    )

let lifecycle = [(Lifecycle.Prototyped, rel_next, "")]

let create =
  call ~name:"create" ~doc:"Create a new VM NVRAM storage cluster for the pool"
    ~result:
      (Ref nvram_store_member, "the newly created NVRAM storage cluster object")
    ~lifecycle ~allowed_roles:_R_POOL_OP
    ~params:
      [
        ( Ref nvram_store
        , "nvram_store"
        , "the nvram storage cluster to be part of"
        )
      ; ( Ref _host
        , "host"
        , "the host to run this NVRAM storage cluster member on"
        )
      ; (Ref _pif, "PIF", "the network interface to use for communication")
      ]
    ()

let destroy =
  call ~name:"destroy" ~doc:"Destroys an NVRAM storage member"
    ~params:
      [(Ref nvram_store_member, "self", "the NVRAM storage member to destroy")]
    ~lifecycle ~allowed_roles:_R_POOL_OP ()

let set_joined =
  call ~flags:[`Session] ~name:"set_joined"
    ~doc:"Add or remove member from NVRAM storage cluster" ~lifecycle
    ~params:
      [
        (Ref nvram_store_member, "self", "the member")
      ; (Bool, "joined", "whether the member should be joined or not")
      ]
    ~allowed_roles:_R_POOL_OP ()

let t =
  create_obj ~name:nvram_store_member ~descr:"VM NVRAM storage server"
    ~doccomments:[] ~gen_events:true ~in_db:true ~lifecycle
    ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_OP ~gen_constructor_destructor:false
    ~contents:
      ([
         uid nvram_store_member ~lifecycle
       ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref nvram_store)
           "nvram_store" ~default_value:(Some (VRef null_ref))
           "Reference to nvram storage cluster"
       ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _host) "host"
           ~default_value:(Some (VRef null_ref)) "Reference to the Host object"
       ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _pif) "PIF"
           ~default_value:(Some (VRef null_ref)) "Reference to the PIF object"
       ; field ~qualifier:StaticRO ~lifecycle
           ~ty:(Map (String, String))
           ~default_value:(Some (VMap [])) "config"
           "Member specific configuration"
       ; field ~qualifier:StaticRO ~lifecycle ~ty:String "peer_url"
           ~default_value:(Some (VString ""))
           "The URL to access the NVRAM storage on the [PIF] network interface"
       ; field ~lifecycle ~ty:Bool "joined"
           ~qualifier:StaticRO (* we provide a custom setter *)
           ~default_value:(Some (VBool false))
           "Whether the member is currently joined to the NVRAM storage cluster"
       ]
      @ allowed_and_current_operations nvram_store_member_operation
      @ [
          field ~qualifier:StaticRO ~lifecycle
            ~ty:(Map (String, String))
            "other_config" ~default_value:(Some (VMap []))
            "Additional configuration"
        ]
      )
    ~messages:[create; set_joined; destroy]
    ()
