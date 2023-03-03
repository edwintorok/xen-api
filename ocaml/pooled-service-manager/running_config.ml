open Types
open Errors

type 'a config_type = (module Config with type t = 'a)

let run_dir = ref (Fpath.v "/var/run/nonpersistent/pooled-service")

let get_run_dir_exn () =
  let dir = !run_dir in
  Xapi_stdext_unix.Unixext.mkdir_rec (Fpath.to_string dir) 0700 ;
  dir

let state_path_exn id =
  let dir = get_run_dir_exn () in
  (* Fpath ensures that [id] doesn't contain '/',
     and protects against directory traversal *)
  Fpath.(dir / id)

let pair k v = (k, v)

let encode_pair (k, v) = Printf.sprintf "%S=%S" k v

let decode_pair line = Scanf.sscanf line "%S=%S" pair

let get_exn (type a) id (module Config : Config with type t = a) =
  let path = state_path_exn id in
  try
    Xapi_stdext_unix.Unixext.read_lines ~path:(Fpath.to_string path)
    |> List.to_seq
    |> Seq.map decode_pair
    |> Astring.String.Map.of_seq
    |> Config.of_dict
    (* not expected to fail here, we serialized it from a valid config*)
    |> error_to_msg
    |> Rresult.R.failwith_error_msg
    |> Option.some
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let write_exn id dict =
  let path = state_path_exn id in
  dict
  |> Astring.String.Map.to_seq
  |> Seq.map encode_pair
  |> List.of_seq
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600
       (Fpath.to_string path)

let remove_exn id =
  id
  |> state_path_exn
  |> Fpath.to_string
  |> Xapi_stdext_unix.Unixext.unlink_safe

let set_exn (type a) id (module Config : Config with type t = a) = function
  | None ->
      remove_exn id
  | Some config ->
      config |> Config.to_dict |> write_exn id
