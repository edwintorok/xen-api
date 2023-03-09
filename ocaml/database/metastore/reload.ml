open Config_field

let src = Logs.Src.create __MODULE__

include (val Logs.src_log src)

(** [fdebug id name k] calls [k] when the debug log-level is enabled,
    similarly to {!Logs.debug}.
    It prefixes all log messages with [name].
*)
let fdebug func k =
  debug @@ fun m ->
  k @@ fun fmt -> m ("%s: " ^^ fmt) func

let etcdctl = ref (Fpath.v "/usr/bin/etcdctl")

let pp_status ppf = function
  | Unix.WEXITED code -> Fmt.pf ppf "exited with code %d" code
  | Unix.WSIGNALED s -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal s
  | Unix.WSTOPPED s -> Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal s

let etcdctl_member args =
  let fdebug fmt = fdebug __FUNCTION__ fmt in
  let cmd, args = Fpath.to_string !etcdctl, "member" :: args in
  let cmdstr = Filename.quote_command cmd args in
  fdebug (fun m -> m "Running %s" cmdstr);
  let print_stdout_stderr (stdout, stderr) =
    fdebug (fun m -> m "Command %s stdout: %a" cmdstr Fmt.lines stdout);
    fdebug (fun m -> m "Command %s stderr: %a" cmdstr Fmt.lines stderr)
  in
  try
    let stdout, stderr = Forkhelpers.execute_command_get_output cmd args in
    print_stdout_stderr (stdout, stderr);
    Ok stdout
  with Forkhelpers.Spawn_internal_error (stderr, stdout, status) ->
    print_stdout_stderr (stdout, stderr);
    Fmt.error_msg "Command %s: %a" cmdstr pp_status status

let get_member_id member_name =
  (* the output here is different between versions, e.g.:
    8e9e05c52164694d: name=default peerURLs=http://localhost:2380 clientURLs=http://localhost:2379 isLeader=true
    8e9e05c52164694d, started, default, http://localhost:2380, http://localhost:2379, false

    Newer versions have a --write-out flag to control output format, but older
    versions do not
    *)
  let+ output = etcdctl_member ["list"] in
  let lines = String.split_on_char '\n' output in
  let list =
    if String.contains output ',' then
      lines |> List.filter_map (fun l -> l |> String.split_on_char ',' |> List.map
      String.trim |> function
        | memberid ::  _ :: name :: _ -> Some (memberid, name)
        | _ -> None
    )
    else if String.contains output '=' then
      lines |> List.map @@ fun l -> Scanf.sscanf l "%s@: name=%s " @@ fun member
      name -> member, name
    else []
  in
  match list |> List.find_opt (fun (_, name) -> String.equal name member_name) with
  | Some (memberid, _) -> Ok memberid
  | None -> Fmt.error_msg "Unable to find own memberid for name=%s" member_name

let get_local_member_id current =
  get_member_id (Config.name current)

module StringMap = Map.Make(String)
let set_of_keys m = m |> StringMap.to_seq |> Seq.map fst |> Astring.String.Set.of_seq

let membership_delta deltamap =
  let is_remove _ entry = Option.is_none entry in
  let to_remove = deltamap |> StringMap.filter is_remove |> set_of_keys
  and to_join = deltamap |> StringMap.filter_map (fun _ -> Fun.id) in
  to_remove, to_join

let remove_member name acc =
  let+ () = acc in
  let+ member_id = get_member_id name in
  let+ (_:string) = etcdctl_member ["remove"; member_id] in
  Ok ()

let add_member name peer_uri acc =
  let+ () = acc in
  let peer_uris_str = peer_uri |> Uri.to_string in
  let+ (_:string) = etcdctl_member ["add"; name; "--peer-urls=" ^ peer_uris_str] in
  Ok ()
  (* TODO: we should start it now, but we're on the wrong member for that ..
     TODO: we should also wait until healthy...
     this is where we need the set_state service API...
   *)

let reload ~next ~current =
  let+ delta, local = Config.diff ~next ~current in
  let update_peer_urls = function
    | [] -> Ok ()
    | peer_urls ->
      let peer_urls_str = peer_urls |> List.map Uri.to_string |> String.concat "," in
      let+ member_id = get_local_member_id current in
      let+ (_:string) = etcdctl_member ["update"; member_id; "--peer-urls=" ^ peer_urls_str] in
      Ok ()
  in
  let need_restart = match delta, local with
  | {advertise_client_urls=[]; initial_advertise_peer_urls=[]; _}, None -> false
  | _ -> true
  in
  let+ () = update_peer_urls delta.initial_advertise_peer_urls in
  let to_remove, to_join = membership_delta delta.initial_cluster in
  (* remove first:
    https://etcd.io/docs/v3.3/faq/#should-i-add-a-member-before-removing-an-unhealthy-member
  *)
  let ok = Ok () in
  let removed = Astring.String.Set.fold remove_member to_remove ok in
  let+ () = StringMap.fold add_member to_join removed in
  Ok (if need_restart then Some next
  else None)


(* TODO: this diffing is too generic,
   add join is better to have own api just for that that queries members and
   updates, otherwise... we may update too many at a time
 *)
