(**
    An abstract key-value store.

    Keys and values have different types to make it clearer in the code
    what refers to keys and what refers to values (if they're both strings code could mistakenly return
    a key when a value is expected).
    They may also have different limits and stored (encoded) differently internally.
*)

module type BoundedString = sig
  (** the type for a string with a maximum length. May be stored encoded in a backend specific way. *)
  type t

  val max_length : int
  (** [max_length] is the maximum length of [t], inclusive. *)

  (*@ ensures max_length > 0 *)

  val of_string_exn : string -> t
  (** [of_string_exn s] creates a {!type:t} value, performing any encoding as necessary.
      
  It may contain arbitrary byte values.
  Backends mustn't assume that keys are valid ASCII, or UTF-8 and perform their own encoding/decoding if necessary.
  An empty string is a valid value.
      
  @raises Invalid_argument if [s] is longer than {!val:max_length}
  *)

  (*@ t = of_string_exn s
      checks (String.length s < max_length)
  *)

  val to_string : t -> string
  (** [to_string t] returns [t] as a string, performing any decoding as necessary. *)
  (*@ forall s. String.length s <= max_length -> String.equal s (s |> of_string_exn |> to_string) *)
end

(** Key-value store.
  
  An implementation of this module type is called a backend.

  Backends impose various limits:
    * on key length, see {!val:Key.max_length}
    * on value lengths, see {!val:Value.max_length}
    * on numbers of keys stored in the backend, see {!val:max_keys}
    * on total amount of data stored in the backend, see {!val:max_data}

  However they  must accept any bytes values [0, 255] as a key or a value.
  If needed the backend should perform its own encoding and decoding,
  and shouldn't assume keys or values are valid UTF-8 or ASCII.

  These limits are the same for all APIs and specific to an instance {!type:t} of a backend.
  
  For example if the configuration is a VM UUID, then a different VM may be considered a different backend
  instance with its own independent limit counting.
*)
module type S = sig
  (** Keys that are valid to be stored in the backend *)
  module Key : BoundedString

  (** Values that are valid to be stored in the backend *)
  module Value : BoundedString

  val max_keys : int
  (** [max_keys] is the maximum number of keys in the backend, inclusive.
      
    This may be temporarily exceeded when executing concurrent {!val:put} calls,
    however when all {!val:put} calls finish the number of keys will be <= max_keys.
  *)

  (*@ ensures max_keys > 0 *)

  val max_data : int
  (** [max_data] is the total maximum amount of data in the backend, inclusive.

    This may be temporarily exceeded when executing concurrent {!val:put} calls,
    however when all {!val:put} calls finish the number of keys will be <= max_data.
    
    The amount of data stored for a key-value pair is defined as:
    [String.length (Key.to_string k) + String.length (Value.to_string v)].
    If this would be too much for the backend (e.g. due to encoding or other overheads) then it should lower [max_data] accordingly.
  *)

  (*@ ensures max_data > 0 *)

  (*@ function kv_length(k: Key.t) (v: Value.t) = String.length (Key.to_string k) + String.length (Value.to_string v) *)

  (** the type used by monadic IO.
    It may implicitly contain a result monad *)
  type +'a io

  val name : string
  (** backend name *)

  (** key-value store configuration *)
  type config

  (** connection to the key-value store *)
  type t

  (*@
    ephemeral
    mutable model keys: Key.t set
    mutable model view: Key.t -> Value.t option
    invariant forall k: Key.t. not (Set.mem k keys) -> view k = None
    invariant forall k: Key.t. view k = None -> not (Set.mem k keys)
    invariant Set.cardinal keys <= max_keys
    invariant Set.fold (fun k acc -> kv_length k (view k) + acc) keys 0 <= max_data
  *)

  val connect : config -> t io
  (** [connect config] establishes a connection to the store.

    When the connection is no longer needed then {!val:disconnect} should be called.
    If a connection cannot be established then either the {!type:io} monad's builtin failure mechanism is used,
    or an exception is raised.

    @param config backend specific configuration (e.g. VM UUID)
    @return a resolved {!type:io} promise for a backend {!type:t} connection.
  *)

  val disconnect : t -> unit io
  (** [disconnect t] closes the connection to [t] and releases any resources.
      It is an error to use [t] after [disconnect] has been called.
      In particular if [disconnect] fails it mustn't be called again.
  *)

  (*@ consumes t *)

  val get : t -> Key.t -> Value.t option io
  (** [get conn key] retrieves the value for [key] if any.
      
      @param conn backend connection
      @param key the {!type:Key.t} to look up.
      @return [Some value] if [key] is present with [value], otherwise [None]
  *)

  (*@ r = get t k
      pure
      ensures r = t.view k *)

  val put : t -> Key.t -> Value.t -> unit io
  (** [put conn key value] sets [key=value].
  
    @param conn backend connection
    @param key the {!type:Key.t} to set
    @param value the {!type:Value.t} to set
    @return a resolved [io] promise when the key has been set
  *)

  (*@
    put t key value
    requires Set.fold (fun k acc -> kv_length k (view k) + acc) keys (kv_length key value) <= max_data
    modifies t
    ensures get t key = Some value
    ensures t.keys = Set.add key (old t.keys)
    ensures forall k. t.view k = if k = key then Some value else old (t.view k)
  *)

  val delete : t -> Key.t -> unit io
  (** [delete conn key] deletes [key].
        It is not an error if they key is already missing.

    @param conn backend connection
    @param key the {!type:Key.t} to delete
    @return a resolved [io] promise when the key has been deleted
  *)

  (*@
    delete t key
    modifies t
    ensures get t key = None
    ensures t.keys = Set.remove key (old t.keys)
    ensures forall k. t.view k = if k = key then None else old (t.view k)
  *)

  val list : t -> Key.t list io
  (** [list conn] returns a list of all keys in the backend.
        The order in which keys are returned is unspecified.
        
    @param conn backend connection
    @return list of keys in unspecified order
  *)
  (*@ l = list t
      pure
      ensures List.length l <= max_keys
      ensures Set.of_list l = keys
  *)
end
