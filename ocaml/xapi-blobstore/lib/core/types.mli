(** 
    An abstract key-value store.
    
    Keys and values have different types to make it clearer in the code
    what refers to keys and what refers to values (if they're both strings code could mistakenly return
    a key when a value is expected).
*)

type key = Key: string -> key
(** type for keys *)

type value = Value: string -> value
(** type for values.
    If extra metadata is needed then it should be serialized into the string,
    e.g. using protobuf.
*)

(** Key-value store signature *)
module type S = sig
    type +'a io
    (** the type used by monadic IO *)

    val name: string
    (** backend name *)
    
    type t (** connection to the key-value store *)
    
    val connect: unit -> t io
    (** [connect ()] establishes a connection to the store.
        Any connection parameters are implicit in the backend.
     *)

    val disconnect: t -> unit io
    (** [disconnect t] closes the connection to [t] and releases any resources. *)
    
    val get : t -> key -> value option io
    (** [get conn key] retrieves the value for [key] if any. *)

    val put: t -> key -> value -> value option io
    (** [put conn key value] sets [key=value] and returns the old value of [key] if any *)
    
    val delete: t -> key -> value option io
    (** [delete conn key] deletes [key], and returns the old value of [key] if any. *)
    
    val list: t -> key list io
    (** [list conn] returns a list of valid keys.
        The order in which keys are returned is unspecified *)
end