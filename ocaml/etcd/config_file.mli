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

val to_environment_file : t -> string
(** [to_environment_file t] converts the configuration [t] to a file suitable
  for systemd [EnvironmentFile]. *)
