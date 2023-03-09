val run_dir : Fpath.t ref
(** [run_dir] the location to store state files. *)

(** a configuration with to and from string conversion functions *)
type 'a config = 'a Serializable.T.t

val set_exn : _ Id.t -> 'a config option -> unit
(** [set_exn id config_type config] stores the [config] configuration for [id],
  using [config_type] to serialize it *)

val get_exn : _ Id.t -> 'a Serializable.typ -> 'a option
(** [get_exn id config_type] retrieves the configuration for [id] if any, using
  [config_type] to deserialize it *)
