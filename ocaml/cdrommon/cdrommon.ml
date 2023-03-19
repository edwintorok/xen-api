(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
let oldnotify = ref false

let disc_inserted name =
  let cmd = Filename.quote_command "xe" ["host-notify"; "type=cdrom"; "params=inserted:" ^ name] in
  let ret = Sys.command cmd in
  (* check if we got an error, and record the fact *)
  match ret with
  | 0 ->
      oldnotify := false
  | _ ->
      oldnotify := true

let disc_removed (_ : string) =
  (* we don't need to do anything *)
  oldnotify := false

let check interval name =
  let has_disc = ref false in
  let check_has_disc status =
    if !has_disc then (
      ( match status with
      | Cdrom.NO_INFO | Cdrom.NO_DISC | Cdrom.TRAY_OPEN ->
          has_disc := false ;
          disc_removed name
      | _ ->
          ()
      ) ;
      if !oldnotify then
        disc_inserted name
    ) else
      match status with
      | Cdrom.DISC_OK ->
          has_disc := true ;
          disc_inserted name
      | _ ->
          ()
  in
  let status = Cdrom.query_cdrom_drive_status name in
  has_disc := status = Cdrom.DISC_OK ;
  while Sys.file_exists name do
    let drive_status = Cdrom.query_cdrom_drive_status name in
    check_has_disc drive_status ;
    Unix.sleep interval
  done

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: %s <cdrompath>\n" Sys.argv.(0) ;
    exit 1
  ) ;
  (* check every 2 seconds *)
  check 2 Sys.argv.(1)
