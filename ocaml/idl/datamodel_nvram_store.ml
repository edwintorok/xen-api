open Datamodel_common
open Datamodel_roles
open Datamodel_types

(* NVRAM storage for UEFI variables and vTPM *)

(* for allowed operations *)
let nvram_store_operation =
  Enum
    ( "nvram_store_operation"
    , [("membership_change", "changing membership of NVRAM storage cluster")]
    )

let backend = Enum ("nvram_backend_kind", [("etcd", "etcd.io backend")])

let lifecycle = []

let create =
  call ~name:"create" ~doc:"Create a new VM NVRAM storage cluster for the pool"
    ~result:(Ref nvram_store, "the newly created NVRAM storage cluster object")
    ~lifecycle ~allowed_roles:_R_POOL_OP
    ~params:[(Ref _network, "network", "the network to use for communication")]
    ()

let destroy =
  call ~name:"destroy" ~doc:"Destroys an NVRAM storage cluster"
    ~params:[(Ref nvram_store, "self", "the NVRAM storage cluster to destroy")]
    ~lifecycle ~allowed_roles:_R_POOL_OP ()

let t =
  create_obj ~name:nvram_store ~descr:"VM NVRAM storage cluster" ~doccomments:[]
    ~gen_events:true ~in_db:true ~lifecycle ~persist:PersistEverything
    ~in_oss_since:None ~messages_default_allowed_roles:_R_POOL_OP
    ~gen_constructor_destructor:false
    ~contents:
      ([
         uid nvram_store ~lifecycle
       ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _network) "network"
           "Reference to the network object"
           ~default_value:(Some (VRef null_ref))
       ; field ~qualifier:StaticRO ~lifecycle
           ~ty:(Map (String, String))
           "config" "Common configuration for storage cluster"
           ~default_value:(Some (VMap []))
       ; field ~qualifier:DynamicRO ~lifecycle
           ~ty:(Set (Ref nvram_store_member)) "nvram_store_members"
           "A list of the NVRAM_store_member objects associated with this \
            NVRAM_store"
       ; field ~qualifier:StaticRO ~lifecycle ~ty:backend "backend"
           ~default_value:(Some (VEnum "etcd"))
           "The type of backend used to store NVRAM"
       ; field ~qualifier:StaticRO ~lifecycle ~ty:String "client_url"
           ~default_value:(Some (VString ""))
           "The URL to access the NVRAM storage from Dom0"
       ]
      @ allowed_and_current_operations nvram_store_operation
      @ [
          field ~qualifier:StaticRO ~lifecycle
            ~ty:(Map (String, String))
            "other_config" ~default_value:(Some (VMap []))
            "Additional configuration"
        ]
      )
    ~messages:[create; destroy] ()
