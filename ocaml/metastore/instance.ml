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

module PolyMap = struct
  module type Key = sig
    type 'a t
  end

  module Make(K: Key) = struct
    module type MapValue = sig
      type a
      type b
      include Map.S with type key = a K.t
      type map = b t
      val t: map
    end

    type (!'a, !'b) t = (module MapValue with type a = 'a and type b = 'b)
  end
end

module type MapS2 = sig
  type a
  type b
  include Map.S with type key = (a, b) t
end

module MakeKey(S2: sig type a type b end) : Map.OrderedType with type t = (S2.a, S2.b) t = struct
  type nonrec t = (S2.a, S2.b) t

  let compare = compare
end
