(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

val create :
     __context:Context.db Context.t
  -> pif:[`PIF] Ref.t
  -> network:[`network] Ref.t
  -> [`network_sriov] Ref.t
(** Create a network-sriov object on the specific PIF and network, it will internally create a logical PIF to connect the network-sriov and network. Topo: physical PIF - network-sriov - logical PIF - network *)

val create_internal :
     __context:Context.db Context.t
  -> physical_PIF:[`PIF] Ref.t
  -> physical_rec:API.pIF_t
  -> network:[`network] Ref.t
  -> [`network_sriov] Ref.t * [`PIF] Ref.t

val destroy : __context:Context.db Context.t -> self:[`network_sriov] Ref.t -> unit
(** Destroy a network-sriov object, and it will automatically destroy the logical PIF that bonded with it. *)

val get_remaining_capacity :
  __context:Context.db Context.t -> self:[`network_sriov] Ref.t -> int64
(** Get the available VF numbers of a SR-IOV object **)
