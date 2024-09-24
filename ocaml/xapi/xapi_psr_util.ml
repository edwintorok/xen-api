(* avoiding circular deps *)
module D = Debug.Make (struct let name = "xapi_psr" end)

module Unixext = Xapi_stdext_unix.Unixext

let checkpoint_path = "/var/lib/xcp/psr_cp"

let old_pool_secret_backup_path = "/var/lib/xcp/ptoken.old"

let new_pool_secret_backup_path = "/var/lib/xcp/ptoken.new"

let read_backups () =
  Backtrace.try_with
    (fun () ->
      ( old_pool_secret_backup_path
        |> Unixext.string_of_file
        |> SecretString.of_string
      , new_pool_secret_backup_path
        |> Unixext.string_of_file
        |> SecretString.of_string
      )
    )
    (fun e bt ->
      D.error
        "xapi_psr_util.ml:read_backups failed (paths='%s', '%s'). reason: %s"
        old_pool_secret_backup_path new_pool_secret_backup_path
        (Printexc.to_string e) ;
      Printexc.raise_with_backtrace
        Api_errors.(
          Server_error (internal_error, ["failed to read backups"], None)
        )
        bt
    )

let load_psr_pool_secrets () =
  match
    ( Sys.file_exists old_pool_secret_backup_path
    , Sys.file_exists new_pool_secret_backup_path
    )
  with
  | false, false ->
      ()
  | false, true | true, false ->
      D.error "expected both files to exist: %s , %s"
        old_pool_secret_backup_path new_pool_secret_backup_path ;
      raise
        Api_errors.(
          Server_error
            (internal_error, ["inconsistent pool secret backup files"], None)
        )
  | true, true ->
      D.info "loading backup pool secrets from psr" ;
      let old_pool_secret, new_pool_secret = read_backups () in
      (* be careful to load them in the correct order *)
      if SecretString.equal (Xapi_globs.pool_secret ()) old_pool_secret then
        Xapi_globs.pool_secrets := [old_pool_secret; new_pool_secret]
      else
        Xapi_globs.pool_secrets := [new_pool_secret; old_pool_secret]
