(** unique identifiers of type ['a] *)
type +'a t

val compare : 'a t -> 'a t -> int
(** [compare a b] is a total order on {!t} *)

val to_string : 'a t -> string
(** [to_string t] converts [t] into a string.
  This string should not include quotes and be suitable for use in filenames.
*)
