(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "pvs_cache_vdi" end)

open D
open Xapi_database.Db_filter_types

let create_vdi ~__context ~sR ~size =
  info "Creating new PVS-cache VDI" ;
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.VDI.create ~rpc ~session_id ~name_label:"PVS cache VDI"
        ~name_description:"PVS cache VDI" ~sR ~virtual_size:size
        ~_type:`pvs_cache ~sharable:false ~read_only:false ~other_config:[]
        ~xenstore_data:[] ~sm_config:[] ~tags:[]
  )

(* Before simply returning the VDI from the DB, check if it actually
   still exists on disk. The VDI may have gone away if it was on a
   non-persistent SR (e.g. on a RAM disk). *)
let get_vdi ~__context ~self =
  let vdi = Db.PVS_cache_storage.get_VDI ~__context ~self in
  (* If there is already an attached VBD for the VDI, then we assume that all is well. *)
  let vbds =
    Db.VBD.get_refs_where ~__context
      ~expr:
        (And
           ( Eq (Field "VDI", Literal (Ref.string_of vdi))
           , Eq (Field "currently_attached", Literal "true")
           )
        )
  in
  if vbds <> [] then
    Some vdi
  else
    let sr = Db.PVS_cache_storage.get_SR ~__context ~self in
    if not (Db.is_valid_ref __context sr) then (
      info "PVS-cache SR %s is no longer present" (Ref.string_of sr) ;
      None
    ) else (
      (* Scan the SR. This will cause the removal of VDI records that are not backed
         by an actual volume on the SR. *)
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.SR.scan ~rpc ~session_id ~sr
      ) ;
      (* If our VDI reference is still valid, then we're good. *)
      if Db.is_valid_ref __context vdi then
        Some vdi
      else (
        info "PVS-cache VDI %s is no longer present on the SR"
          (Ref.string_of vdi) ;
        None
      )
    )

let m = Mutex.create ()

let get_or_recreate_vdi ~__context ~self =
  Xapi_stdext_threads.Threadext.Mutex.execute m (fun () ->
      match get_vdi ~__context ~self with
      | None ->
          let sR = Db.PVS_cache_storage.get_SR ~__context ~self in
          let size = Db.PVS_cache_storage.get_size ~__context ~self in
          let vdi = create_vdi ~__context ~sR ~size in
          Db.PVS_cache_storage.set_VDI ~__context ~self ~value:vdi ;
          vdi
      | Some vdi ->
          vdi
  )

let ensure_vdi_is_closed ~__context vdi =
  (* The pvsproxy daemon normally unmounts and removes the locally attached VBD
     for the cache VDI. Here we do it as well, just in case pvsproxy did not
     manage to, for whatever reason. *)
  try
    let uuid = Db.VDI.get_uuid ~__context ~self:vdi in
    let _ : string =
      Helpers.call_script !Xapi_globs.pvsproxy_close_cache_vdi [uuid]
    in
    ()
  with _ -> () (* call_script will log anything interesting already *)

let destroy_vdi ~__context ~self =
  match get_vdi ~__context ~self with
  | None ->
      () (* The VDI doesn't exist anymore; nothing to do. *)
  | Some vdi ->
      ensure_vdi_is_closed ~__context vdi ;
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.VDI.destroy ~rpc ~session_id ~self:vdi
      )
