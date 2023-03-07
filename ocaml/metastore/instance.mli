(** an instance of a service or xapi object *)

type ('a, 'b) t  (** an instance with configuration of type ['a] and identifier type ['b] *)

(* type ('a, 'b, 'c) command = ('a, 'b) t -> 'b Vtasks.t*)

val v: 'a Serializable.typ -> 'b Id.t -> 'a -> ('a, 'b) t
(** [v typ_of id config] is a unique instance [id] with configuration [config]. *)

val config: ('a, 'b) t -> 'a
(** [config t] is the configuration of [t] *)

val id: ('a, 'b) t -> 'b Id.t
(** [id t] is the unique id of [t] *)

val compare: (_,'a) t -> (_, 'a) t -> int
(** [compare a b] is a total order on {!t} based on its unique identifier *)

val dump: (_, _) t Fmt.t
(** [dump t] pretty prints a representation of [t] for debugging *)
