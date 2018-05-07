(*
 * Copyright (C) 2006-2018 Citrix Systems Inc.
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
module R = Debug.Make(struct let name = "redo_log" end)

let read_from_redo_log log staging_path db_ref =
  Redo_log_replay.replay_from_redo_log log staging_path (Datamodel_schema.of_datamodel ()) db_ref

let stop_using_redo_log log =
  R.debug "Stopping using redo log";
  try
    Redo_log.shutdown log
  with _ -> () (* best effort only *)

