type 'a config = 'a Serializable.T.t

let run_dir = ref (Fpath.v "/var/run/nonpersistent/pooled-service")

let get_run_dir_exn () =
  let dir = !run_dir in
  Xapi_stdext_unix.Unixext.mkdir_rec (Fpath.to_string dir) 0700 ;
  dir

let state_path_exn id =
  let dir = get_run_dir_exn () in
  (* Fpath ensures that [id] doesn't contain '/',
     and protects against directory traversal *)
  Fpath.(dir / Id.to_string id)

let get_exn id typ =
  let path = state_path_exn id in
  try
    path |> Fpath.to_string
    |> Xapi_stdext_unix.Unixext.string_of_file
    |> Serializable.deserialize typ
    |> Rresult.R.failwith_error_msg
    |> Option.some
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let write_exn id (typ_of, config) =
  let path = state_path_exn id in
  config |> Serializable.serialize typ_of
  |> Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600
       (Fpath.to_string path)

let remove_exn id =
  id
  |> state_path_exn
  |> Fpath.to_string
  |> Xapi_stdext_unix.Unixext.unlink_safe

let set_exn id  = function
  | None ->
      remove_exn id
  | Some config -> write_exn id config
