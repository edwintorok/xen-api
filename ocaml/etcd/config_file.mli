(** ETCD configuration file *)
type t

val empty : t
(** [empty] is the empty configuration file *)

(** [FieldError (field, exn)] is a wrapper around exceptions encountered while
  converting fields and field values to give more context on which field caused
  the exception *)
exception FieldError of (string * exn)

val add_exn : 'a Config_field.t -> 'a -> t -> t
(** [add_exn field value t] adds the [field] with [value] to [t], overwriting
  any previous value of [field] if any.
  Passes through any exceptions raised when converting [value] to [string].
*)

val union_exn: t -> t -> t
(** [union_exn a b] is the union of configurations [a] and [b].
  @raises Invalid_argument on duplicate keys with conflicting values
*)

val of_dict: (string * string) list -> t
(** [of_dict dict] converts [dict] into a configuration *)

val to_dict : t -> (string * string) list
(** [to_dict config] converts [config] into a key-value pair list. *)

val delta: current:t -> desired:t -> (string * string option) list
(** [delta ~current ~desired] is the list of fields changed to reach [desired] configuration from [current]. *)

val to_environment_file : t -> string
(** [to_environment_file t] converts the configuration [t] to a file suitable
  for systemd [EnvironmentFile]. *)
