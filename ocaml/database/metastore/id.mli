(** Unique identifiers implemented with {!Uuidm}

  See {{!page-metastore_design.section-"unique-identifiers"} the design}.
*)

include Map.OrderedType with type t = Uuidm.t

(** A {!module:Map} with {!t} identifiers as keys *)
module Map : sig
  include Map.S with type key = Uuidm.t

  val dump : 'a Fmt.t -> 'a t Fmt.t
  (** [dump pp_val ppf t] dumps a representation of [t] on [ppf] for debugging,
      using [pp_val] to format values. *)
end

(** A {!module:Set} with {!t} identifiers as keys *)
module Set : sig
  include Set.S with type elt = Uuidm.t

  val dump : t Fmt.t
  (** [dump ppf t] dumps a representation of [t] on [ppf] for debugging
      purposes. *)
end
