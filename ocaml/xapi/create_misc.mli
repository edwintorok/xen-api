(*
 * Copyright (C) 2015 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type host_info = {
    name_label: string
  ; xen_verstring: string option
  ; linux_verstring: string
  ; hostname: string
  ; uuid: string
  ; dom0_uuid: string
  ; oem_manufacturer: string option
  ; oem_model: string option
  ; oem_build_number: string option
  ; machine_serial_number: string option
  ; machine_serial_name: string option
  ; total_memory_mib: int64 option
  ; cpu_info: Xenops_interface.Host.cpu_info option
  ; chipset_info: Xenops_interface.Host.chipset_info option
  ; hypervisor: Xenops_interface.Host.hypervisor option
}

val read_localhost_info : __context:Context.db Context.t -> host_info
(** [read_localhost_info ~__context] creates a [host_info] record with fresh
    information from the system. Use the database to query information instead
    unless you're absolutely necessary. There is no guarantee the information
    gathered by this function the same as the one in the database. *)

val ensure_domain_zero_records :
  __context:Context.db Context.t -> host:[`host] Ref.t -> host_info -> unit

val create_root_user : __context:Context.db Context.t -> unit

val create_software_version :
  __context:Context.db Context.t -> ?info:host_info option -> unit -> unit
(** [create_software_version ~__context ?info ()] creates a version of the
    distribution using [info] if [info] is [Some value]. Otherwise it creates
    the version using fresh information from the system. *)

val create_host_cpu : __context:Context.db Context.t -> host_info -> unit

val create_pool_cpuinfo : __context:Context.db Context.t -> unit

val create_chipset_info : __context:Context.db Context.t -> host_info -> unit

val create_updates_requiring_reboot_info :
  __context:Context.db Context.t -> host:[`host] Ref.t -> unit
