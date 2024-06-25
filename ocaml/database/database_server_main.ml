open Xapi_stdext_unix

type mode =
  | Slave of string
  (* master IP *)
  | Master of string

(* database filename *)

let mode = ref None

let finished = ref false

let m = Mutex.create ()

let c = Condition.create ()

(** Handler for the remote database access URL *)
let remote_database_access_handler_v1 req bio =
  try Xapi_database.Db_remote_cache_access_v1.handler req bio
  with e ->
    Printf.printf "Caught: %s\n" (Printexc.to_string e) ;
    Printexc.print_backtrace stdout ;
    flush stdout ;
    raise e

(** Handler for the remote database access URL *)
let remote_database_access_handler_v2 req bio =
  try Xapi_database.Db_remote_cache_access_v2.handler req bio
  with e ->
    Printf.printf "Caught: %s\n" (Printexc.to_string e) ;
    Printexc.print_backtrace stdout ;
    flush stdout ;
    raise e

let schema = Test_schemas.schema

let _ =
  let listen_path = ref "./database" in
  let self_test = ref false in
  Printexc.record_backtrace true ;
  Arg.parse
    [
      ( "--slave-of"
      , Arg.String (fun master -> mode := Some (Slave master))
      , "run as a slave of a remote db"
      )
    ; ( "--debug"
    , Arg.Unit (fun () -> Debug.log_to_stdout ())
    , "log debug messages to stderr"
    )
    ; ( "--master"
      , Arg.String (fun db -> mode := Some (Master db))
      , "run as a master from the given db filename"
      )
    ; ( "--listen-on"
      , Arg.Set_string listen_path
      , Printf.sprintf "listen for requests on path (default %s)" !listen_path
      )
    ; ("--test", Arg.Set self_test, "Run unit tests in-process")
    ]
    (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n%!" x)
    "run a stand-alone database server" ;
  match !mode with
  | None ->
      failwith "Requires either --slave-of or --master arguments"
  | Some mode -> (
    match mode with
    | Slave master ->
        if !self_test then begin
          (* TODO: parse host:port *)
          Xapi_database.Master_connection.get_master_address := (fun () -> master);          
          Xapi_database.Master_connection.use_fork_exec_helper := false; 
          Xapi_database.Db_globs.https_port := 8443;
          (* the test triggers a 500 error on purpose *)
          Xapi_database.Db_globs.permanent_master_failure_retry_interval := 0.001;
          Xapi_database.Master_connection.master_rpc_path := "/remote_db_access";
          Xapi_database.Master_connection.open_secure_connection ();
          Xapi_database.Database_test.Tests.main false
        end
        else
          failwith "unimplemented"
    | Master db_filename ->
        let open Xapi_database in
        Printf.printf "Database path: %s\n%!" db_filename ;
        let db = Parse_db_conf.make db_filename in
        Db_conn_store.initialise_db_connections [db] ;
        let t = Db_backend.make () in
        Db_cache_impl.make t [db] schema ;
        Db_cache_impl.sync [db] (Db_ref.get_database t) ;
        (* TODO: parse path vs http:port *)
        (*
        Unixext.unlink_safe !listen_path ;
        let sockaddr = Unix.ADDR_UNIX !listen_path in*)
        let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8080) in
        let socket = Http_svr.bind sockaddr "unix_rpc" in
        let server = Http_svr.Server.empty () in
        Http_svr.Server.add_handler server Http.Post "/remote_db_access"
          (Http_svr.BufIO remote_database_access_handler_v1) ;
        Http_svr.Server.add_handler server Http.Post "/remote_db_access_v2"
          (Http_svr.BufIO remote_database_access_handler_v2) ;
        Http_svr.start ~conn_limit:1024 server socket ;
        Printf.printf "server listening\n%!" ;
        if !self_test then (
          Printf.printf "Running unit-tests\n%!" ;
          Xapi_database.Database_test.Tests.main true ;
          Printf.printf "All tests passed\n%!" ;
          finished := true
        ) ;
        (* Wait for either completion *)
        Xapi_stdext_threads.Threadext.Mutex.execute m (fun () ->
            while not !finished do
              Condition.wait c m
            done
        ) ;
        Http_svr.stop socket
  )
