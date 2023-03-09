open Types

module Make (L : Lifecycle) :
  StatefulService
    with type id_kind = L.id_kind
     and type config = L.config
