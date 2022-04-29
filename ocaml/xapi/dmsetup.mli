module Device : sig
  type 'a t

  val of_block : string -> [> `dev] t
  (** [of_block path] checks that [path] refers to a blockdevice and returns a device type.
      @raises Invalid_argument otherwise *)

  val of_regular : string -> [> `losetup] t
  (** [of_regular path] checks that [path] refers to a regular file, and sets up a loopback
      blockdevice on top of it as needed (reusing existing `losetup` devices).
      @raises Invalid_argument otherwise *)
end

module Table : sig
  (** a Linux device-mapper mapping type *)
  type t

  val linear :
       ?logical_start_sector:int64
    -> ?start_sector:int64
    -> 'a Device.t
    -> num_sectors:int64
    -> t
  (** [linear ?logical_start_sector ?start_sector destination_device ~num_sectors] maps the sector
    range [logical_start_sector, logical_start_sector+num_sectors) to [destination_device] range
    [start_sector, start_sector+num_sectors] *)
end

(** a Linux device-mapper device. 
    The type parameter can be:
      [unit] for an active device
      [`suspended] for a suspended device
 *)
type 'a t = [`dmsetup of 'a] Device.t

type name = Uuidm.t

val create : name -> Table.t -> unit t
(** [create name table] runs [dmsetup create] and creates a device [name] with [table].
    [name] must be unique. *)

val remove : ?force:bool -> unit t -> unit
(** [remove ?force name] removes the device [name].
    When force is specified and the device is still in use it is replaced with a
    table that fails all IO and removed once the last user closes it.
    This ensures that any underlying mapped devices can be detached.
    Otherwise removal fails after a few retries (which are needed due to udev possibly interfering)
    *)

val info : _ t -> unit
(** [info name] logs information about status of the device [name] *)

val with_suspended : unit t -> ([< `suspended] t -> 'a) -> 'a
(** [with_suspended t f] suspends [t], runs [f] and resumes [t]. If [f] raises an exception [t] is
    still resumed *)

val reload : [< `suspended] t -> Table.t -> unit
(** [reload t table] replaces the mapping table in [t]. [t] should be suspended. *)

val unplug_users : ?force:bool -> _ Device.t -> unit
(** [unplug_users ?force device] will remove all mappings for [device] allowing [device] to be unplugged.
    See {!remove} for the semantics of [force] *)
