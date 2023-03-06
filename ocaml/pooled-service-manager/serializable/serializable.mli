(** Helpers for serializing and deserializing data structures. *)
(** a value together with a description of how to serialize it.

  Using this type avoids having to functorize everything that uses serializable types.
 *)
type 'a t = 'a Rpc.Types.typ * 'a

(** a module defining a type that can be serialized and deserialized safely. *)
module type S = sig
  (** a serializable type *)
  type t

  val typ_of : t Rpc.Types.typ
  (** description how to convert type to and from {!Rpc.t} *)
end

val v : 'a Rpc.Types.typ -> 'a -> 'a t
(** [v typ_of t] is a convenience function to construct a {!t} type. *)

val of_module : (module S with type t = 'a) -> 'a -> 'a t
(** [of_module (module S) v] convenience function for passing around a value
    together with its serialization functions.

    @param [module S] describes the type and how to convert it to/from {!Rpc.t}
    @param v the value

    @returns [v] with [module S]
 *)

val dump : _ t Fmt.t
(** [dump formatter t] dumps a representation of [t] for debugging purposes. *)

val typ_of_stringmap : 'a Rpc.Types.typ -> 'a Map.Make(String).t Rpc.Types.typ
(** [typ_of_stringmap typ_of_el] is a string map {!Rpc.Types.typ} that can be
    used to serialize and deserialize string maps with [typ_of_el] value types. *)

val serialize : _ t -> string
(** [serialize typ t] creates a string serialization of [t].
  As long as [typ] is changed in compatible ways new versions of the library
  can deserialize data written by old versions safely (i.e. it does NOT use [Marshal]).

  Compatible changes: introducing new fields in a record that are optional, or
  have an [[@default]] value.
*)

val deserialize :
  'a Rpc.Types.typ -> string -> ('a t, [> Rpcmarshal.err]) result
(** [deserialize typ str] deserializes [str] using [typ].
    If data is an incorrect format it returns an [Error] with an appropriate
    message, this shouldn't raise exceptions.
 *)

val of_file :
     'a Rpc.Types.typ
  -> Fpath.t
  -> ('a t, [> Rpcmarshal.err | Rresult.R.exn_trap]) result
(** [of_file path] loads the contents of [path] as a (binary) string. *)

val to_file : Fpath.t -> _ t -> (unit, [> Rresult.R.exn_trap]) result
(** [to_file path contents] writes [contents] to [path].
    On a successful return [path] is expected to have persisted [contents] to
    survive a crash (provided that [path] is not on a RAM filesystem)
 *)
