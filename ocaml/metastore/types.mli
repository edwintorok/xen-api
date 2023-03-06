(** a generic configuration type *)
type 'a config = 'a Serializable.t

(** module for unique identifiers, e.g. Uuidm, _ Ref.t, or sets of them *)
module type Id = sig
  include Map.OrderedType

  include Serializable.S with type t := t
end
