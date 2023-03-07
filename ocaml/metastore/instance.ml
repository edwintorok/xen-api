type ('a, 'b) t =
  { config: 'a
  ; typ_of_config: 'a Serializable.typ
  ; id: 'b Id.t
  }

let v typ_of_config id config = { config; typ_of_config; id }

let compare a b = Id.compare a.id b.id

let id t = t.id
let config t = t.config

let config_serializable t = Serializable.T.v t.typ_of_config t.config

let dump ppf t =
  let open Fmt.Dump in
  record
  [ field "id" id Id.dump
  ; field "config" config_serializable Serializable.T.dump
  ] ppf t
