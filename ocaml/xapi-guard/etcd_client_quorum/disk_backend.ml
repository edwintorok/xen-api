open Etcd_rpc_types
open Lwt.Syntax

let db_path = Fpath.v "/var/tmp/x-kvstorage"

let created =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir (Fpath.to_string db_path) 0o700)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit | e -> Lwt.fail e
      )

let alphabet = Base64.uri_safe_alphabet (* filename safe alphabet *)

let path_of_key key =
  Fpath.(db_path / Base64.encode_exn ~pad:false ~alphabet key)

let revisions = ref 0 (* TODO: this has to be persistent too *)

let make_response_header () =
  let revision = !revisions |> Int64.of_int in
  Some (Etcd_rpc_types.default_response_header ~revision ())

let with_fd path flags perm f =
  let* fd = Lwt_unix.openfile (Fpath.to_string path) flags perm in
  Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

let fsync_dir dir =
  (* directories can only be opened RO, it is strange that you can then call a
     write operation (fsync) on something that is RO, but that is how the API
     works.
     This is needed to make renames crash-proof, otherwise according to POSIX you
     could end up with neither the old or new file after a crash, or some other
     invalid state (e.g. XFS used to truncate files to 0 bytes post crash
     sometimes)
  *)
  with_fd dir [Unix.O_RDONLY] 0 @@ fun fd -> Lwt_unix.fsync fd

let write_atomic path contents =
  (* we cannot use Lwt_io here, because it doesn't give us access to the
     internal file descriptor needed for fsync
     we also cannot use the temp file creation in Filename, because those would
     do a blocking syscall, blocking every other request (from other VMs, etc.)

     This won't work, becau
  *)
  let filename = Fpath.(add_ext ".tmp" path) in
  (* TODO: unique suffix *)
  Logs.debug (fun m -> m "Attempting to write to %a" Fpath.pp filename) ;
  let* () =
    with_fd filename [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_EXCL] 0o700
    @@ fun fd ->
    let expected = String.length contents in
    let* written = Lwt_unix.write_string fd contents 0 expected in
    (* this should match because Lwt should internally retry on EINTR *)
    if written <> expected then (
      (* TODO: use Lwt version of logger *)
      Logs.err (fun m -> m "Short write? %d <> %d" written expected) ;
      Lwt.fail_with "short write"
    ) else
      Lwt_unix.fdatasync fd
  in
  let* () = Lwt_unix.rename (Fpath.to_string filename) (Fpath.to_string path) in
  fsync_dir (Fpath.parent path)

let read_path key path =
  Lwt.catch
    (fun () ->
      let pathstr = Fpath.to_string path in
      Logs.debug (fun m -> m "attempting to read %s" pathstr) ;
      Lwt_io.with_file ~mode:Lwt_io.Input pathstr @@ fun ic ->
      let+ value = Lwt_io.read ic in
      (* TODO: we should store and retrieve the other fields too *)
      Kv_types.default_key_value ~key ~value () |> Option.some
    )
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none | e -> Lwt.fail e
      )

(* an in-memory backend for benchmarking *)
let put (put : put_request) =
  let path = path_of_key put.key in
  (* TODO: check that there are no other fields set that we do not support *)
  let* prev_kv =
    if put.prev_kv then
      read_path put.key path
    else
      Lwt.return_none
  in
  let* () = created in
  let* () = write_atomic path put.value in
  incr revisions ;
  Lwt_result.return {header= make_response_header (); prev_kv}

let range (range : range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let* kvopt = range.key |> path_of_key |> read_path range.key in
  let kvs = Option.to_list kvopt in
  Lwt_result.return
    {
      header= make_response_header ()
    ; kvs
    ; more= false
    ; count= List.length kvs |> Int64.of_int
    }

let delete_range (deleterange : delete_range_request) =
  (* TODO: check that there are no other fields set that we do not support *)
  let path = path_of_key deleterange.key in
  let* prev_kv =
    if deleterange.prev_kv then
      read_path deleterange.key path
    else
      Lwt.return_none
  in
  let* exists = Lwt_unix.file_exists (Fpath.to_string path) in
  let* () = path |> Fpath.to_string |> Lwt_unix.unlink in
  Lwt_result.return
    {
      header= make_response_header ()
    ; prev_kvs= Option.to_list prev_kv
    ; deleted= (if exists then 1L else 0L)
    }
