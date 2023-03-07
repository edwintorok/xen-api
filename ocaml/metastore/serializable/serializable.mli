(** Helpers for serializing and deserializing data structures. *)

(** shortened typename *)
type 'a typ = 'a Rpc.Types.typ

module T : sig
  (** a value together with its serializer *)
  type 'a t = 'a typ * 'a

  val v : 'a typ -> 'a -> 'a t
  (** [v typ_of t] is a pair of [t] and its serializer [typ_of] *)

  val dump : _ t Fmt.t
  (** [dump t] pretty prints a representation of [t] for troubleshooting *)
end

(** a module defining a type that can be serialized and deserialized safely. *)
module type S = sig
  (** a serializable type *)
  type t

  val typ_of : t typ
  (** description how to convert type to and from {!Rpc.t} *)
end

val using : aname:string -> ('a -> 'b) -> ('b -> 'a) -> 'b typ -> 'a typ
(** [using aname to_other of_other typ_of_other] is a {!typ} implemented by
    converting [v] to and from [typ_other] using [to_other] and [of_other].
    [aname] is the name given to the abstract type.

 *)

val dump : 'a typ -> 'a Fmt.t
(** [dump typ_of formatter t] dumps a representation of [t] for debugging purposes
    using [typ_of]. *)

val typ_of_stringmap : 'a typ -> 'a Map.Make(String).t typ
(** [typ_of_stringmap typ_of_el] is a string map {!typ} that can be
    used to serialize and deserialize string maps with [typ_of_el] value types. *)

val serialize : 'a typ -> 'a -> string
(** [serialize typ_of t] creates a string serialization of [t].
  As long as [typ_of] is changed in compatible ways new versions of the library
  can deserialize data written by old versions safely (i.e. it does NOT use [Marshal]).

  Compatible changes: introducing new fields in a record that are optional, or
  have an [[@default]] value.
*)

val deserialize : 'a typ -> string -> ('a, [> Rpcmarshal.err]) result
(** [deserialize typ_of str] deserializes [str] using [typ_of].
    If data is an incorrect format it returns an [Error] with an appropriate
    message, this shouldn't raise exceptions.
 *)

val of_file :
  'a typ -> Fpath.t -> ('a, [> Rpcmarshal.err | Rresult.R.exn_trap]) result
(** [of_file typ_of path] loads the contents of [path] as a (binary) string. *)

val to_file : 'a typ -> Fpath.t -> 'a -> (unit, [> Rresult.R.exn_trap]) result
(** [to_file typ_of path contents] writes [contents] to [path].
    On a successful return [path] is expected to have persisted [contents] to
    survive a crash (provided that [path] is not on a RAM filesystem)
 *)
