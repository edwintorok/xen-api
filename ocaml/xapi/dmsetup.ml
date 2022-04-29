open Cmd
open Xapi_stdext_threads.Stdext

module D = Debug.Make (struct let name = "xapi_dmsetup" end)

let losetup_m = Mutex.create ()

let losetup args =
  let cmd = !Xapi_globs.losetup in
  run ~cmd Forkhelpers.execute_command_get_output args |> fst

let losetup_find file =
  Mutex.execute losetup_m @@ fun () ->
  losetup @@ (v "--find" % "--nooverlap" % "--direct-io=on" % file % "--show")

let losetup_detach file =
  Mutex.execute losetup_m @@ fun () ->
  let device = losetup_find file in
  losetup @@ (v "--detach" % device)

module Device = struct
  open Unix.LargeFile

  type 'a t = Losetup of string | Block of string

  let of_path kind kindstr path =
    match stat path with
    | s ->
        if s.st_kind = kind then
          Block path
        else
          Fmt.invalid_arg "Not a %s: %s" kindstr path
    | exception (Unix.Unix_error _ as e) ->
        invalid_arg (Printexc.to_string e)

  let of_block = of_path Unix.S_BLK "block device"

  let of_regular path =
    Losetup (path |> of_path Unix.S_REG "regular file" |> losetup_find)

  let to_blockdev (Losetup dev | Block dev) = dev

  let iter_losetup f = function Block _ -> () | Losetup dev -> f dev
end

module Table = struct
  type t = {
      logical_start_sector: int64
    ; num_sectors: int64
    ; target_type: string
    ; target_args: string list
  }

  let make ?(logical_start_sector = 0L) ~num_sectors target_type target_args =
    {logical_start_sector; num_sectors; target_type; target_args}

  let linear ?(logical_start_sector = 0L) ?(start_sector = 0L)
      destination_device =
    make ~logical_start_sector "linear"
      [Device.to_blockdev destination_device; Int64.to_string start_sector]
end

type name = Uuidm.t

type t = name

let table t =
  let open Table in
  let args = String.concat " " t.target_args in
  v "--table"
  % Printf.sprintf "%Lu %Lu %s %s" t.logical_start_sector t.num_sectors
      t.target_type args

(* we expose only most important parameters, more can be added as needed *)
let uuidm u = v "--uuid" % Uuidm.to_string u

(* use UUIDs as names *)
let device uuid = "/dev/mapper/xapi_dmsetup_" ^ Uuidm.to_string uuid

(* avoid evaluating Xapi_globs too early, do not partially apply [run] *)
(* TODO: remove --checks *)

let syslog_stdout = Forkhelpers.Syslog_WithKey "dmsetup"

let dmsetup_out subcmd name args =
  let cmdargs = v subcmd % device name % "--checks" %% args in
  let cmd = !Xapi_globs.dmsetup in
  let out, err =
    cmdargs |> run ~cmd @@ Forkhelpers.execute_command_get_output ~syslog_stdout
  in
  if String.length err > 0 then
    D.warn "%s %s stderr: %s" cmd (Fmt.to_to_string pp cmdargs) err ;
  out

let dmsetup subcmd name args =
  let (_ : string) = dmsetup_out subcmd name args in
  ()

let major_minor device =
  (* we could also bind to major(3) and minor(3)
     Note that the minor number is not limited to 255.
     FIXME: there is buggy code in the rest of XAPI that assumes that
     and would break on minor devices >255 (e.g. when we have more >255 loop devices)
  *)
  run ~cmd:!Xapi_globs.stat @@ (v "--format" % "%Hd:%Ld") |> fst

let find device =
  let devno = major_minor device in
  dmsetup_out "info"
  @@ (v "-C" % "--noheadings" % "--select" % "devnos_used=")
  ^ (devno % "-o" % "name")
  |> String.split_on_char '\n'

let create name tbl =
  (* do not use --uuid here, just in case it can be made to collide with some other device's UUID on
     the system, e.g. on import with preserve=true *)
  dmsetup "create" name @@ (v "--addnodeoncreate" %% table tbl) ;
  name

let remove ?(force = false) name =
  dmsetup "remove" name
  @@ if force then v "--force" % "--deferred" else v "--retry"

let info name = dmsetup "info" name empty

let suspend name = dmsetup "suspend" name empty

let resume name = dmsetup "resume" name @@ v "--addnodeonresume"

let reload name tbl = dmsetup "reload" name @@ table tbl

let with_suspended name f =
  let () = suspend name in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f name)
    (fun () -> resume name)

let unplug_users ?force device =
  D.debug "Finding and removing any mappings using device %s"
  @@ Device.to_blockdev device ;
  device |> find |> List.iter (remove ?force) ;
  Device.iter_losetup losetup_detach device
