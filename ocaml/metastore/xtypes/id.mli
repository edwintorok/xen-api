(** unique identifiers of type ['a] *)
type 'a t = 'a Ref.t

val compare : 'a t -> 'a t -> int
(** [compare a b] is a total order on {!t} *)

val to_string : 'a t -> string
(** [to_string t] converts [t] into a string.
  This string should not include quotes and be suitable for use in filenames.
*)

val dump : 'a t Fmt.t
(** [dump formatter t] pretty prints a representation of [t] *)

(** an ['a t] that hides ['a] and makes it suitable for Map.Make()/Set.Make() *)
module Key : Map.OrderedType

val key: 'a t -> Key.t
(** [key id] a constructs {Key.t} suitable for use in Map and Set lookups. *)

module Map: Map.S with type key = Key.t
(** a map using unique identifiers as keys *)
