open Types

type 'a config_type = (module Config with type t = 'a)
(** a configuration with to and from string conversion functions *)

val set_exn: Uuidm.t -> 'a config_type -> 'a option -> unit
(** [set_exn id config_type config] stores the [config] configuration for [id],
  using [config_type] to serialize it *)

val get_exn: Uuidm.t -> 'a config_type -> 'a option
(** [get_exn id config_type] retrieves the configuration for [id] if any, using
  [config_type] to deserialize it *)
