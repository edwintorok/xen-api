type command = string * string list

type live
type global
type local

module Section: sig
  type 'a t
  (** a section of the configuration that can be updated in the same way, e.g.
  member restart, etcdctl command, etc. *)

  type 'a kind (** Configuration section kind, one of {!live}, {!global}, {!local}. *)

  val live: live kind (** configuration that needs to be updated by telling other running members about it, or using bootstrap config *)
  val global: global kind (** configuration that is the same across the entire etcd cluster *)
  val local: local kind (** configuration that is local to an etcd member node *)

  val union_exn: 'a t -> 'a t -> 'a t
  (** [union_exn a b] is the merged configuration of [a] and [b].
    @raises Invalid_argument if there are duplicate keys
  *)

  val of_field_value_exn: 'a kind -> 'b Config_field.t -> 'b -> 'a t
  (** [of_field_value_exn kind field value] is the configuration entry for [field=value].

    @raises Invalid_argument if [field] is not of type [kind]
  *)

  val of_dict: 'a kind -> (string * string) list -> ('a t, [`Msg of string]) result
  (** [of_dict kind dict] converts [dict] into a configuration section of type [kind].
    If this is not possible then an error is returned.
  *)

  val to_dict: 'a kind -> 'a t -> (string * string) list
  (** [to_dict kind section] converts the configuration to a dictionary.
    [kind] is used to check that the correct type of configuration is stored.
  *)

  val delta: current:'a t -> desired:'a t -> 'a t
  (** [delta ~current ~desired] computes the needed changes to get from [current] to [desired].
    It is the caller's responsiblity to hold a lock
    (to prevent conflicting requests) and iterate until a fixed point is reached.
  *)

  val apply_live: live t -> command list
  (** [apply_live live_delta] is a list of commands to execute to apply [live_delta]. *)
end

type t =
{ live: live Section.t (** Live configuration to change, or bootstrap configuration to start new cluster *)
; global: global Section.t (** Configuration that all members in the cluster need to agree on. Usually it cannot be changed live. *)
; local: local Section.t (** Configuration local to a member, that can be changed by simply restarting the member *)
}

val to_environment_file: t -> string
(** [to_environment_file config] is {!Config.to_environment_file} on the config file generated
  out of the bootstrap, global and local configurations *)
