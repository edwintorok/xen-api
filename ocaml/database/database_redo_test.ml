open Db_cache_types
open Xapi_stdext_monadic

let check_database msg a b =
  let canonical db =
    Db_backend.blow_away_non_persistent_fields (Database.schema db) db
  in
  let to_string db =
    let buf = Buffer.create 4096 in
    let out = Xmlm.make_output ~indent:(Some 2) (`Buffer buf) in
    Db_xml.To.database out db ; Buffer.contents buf
  in
  let a_str = a |> canonical |> to_string
  and b_str = b |> canonical |> to_string in
  Alcotest.(check string msg a_str b_str)


let event = Alcotest.of_pp @@ Fmt.using sexp_of_update Sexplib.Sexp.pp_hum

let check_database_events msg (db1, events1) (db2, events2) =
  Alcotest.(check (list event) msg events1 events2) ;
  check_database msg db1 db2


let is_persistent_event update db =
  match update with
  | RefreshRow _ | PreDelete _ | AfterLockRelease -> false
  | WriteField (tblname, objref, fldname, _, _) ->
    Schema.is_field_persistent (Database.schema db) tblname fldname
  | Delete (tblname, objref, _) | Create (tblname, objref, _) ->
    Schema.is_table_persistent (Database.schema db) tblname


let persistent_events_of db f =
  let events = ref [] in
  let event update db =
    prerr_endline "event";
    if is_persistent_event update db then events := update :: !events
  in
  let callback = "events test trace" in
  db |> Database.register_callback callback event |> f
  |> Database.unregister_callback callback |> fun db -> (db, !events)


let assert_healthy healthy =
  (* not supposed to become unhealthy during unit test *)
  if not healthy then failwith "state_change_callback: redo log is unhealthy"


let recover redo_log schema db =
  let tmp = ref db in
  let recovered = Db_ref.in_memory (ref tmp) in
  (* fixme: redo log replay looses callbacks *)
  let orig_db = db in
  let callback = "test replay", (fun update db -> Database.notify update orig_db) in
  Redo_log_replay.read_from_redo_log ~callback redo_log "recovered.db" recovered
    Test_schemas.schema ;
  Redo_log.shutdown redo_log ;
  Db_ref.get_database recovered


let recover_events redo_log schema =
  persistent_events_of (Database.make schema) (recover redo_log schema)


let test_redo_log =
  Redo_log.create ~name:"test" ~state_change_callback:(Some assert_healthy)
    ~read_only:false


(* Check that database replay events on persistent fields/tables and the final database matches
 * the original.
 * Note: this will fail if flush is called because the individual events are not preserved in that case
 * *)
let check_database_replay msg ~redo_log_name db f =
  Xapi_stdext_unix.Unixext.touch_file redo_log_name;
  Unix.LargeFile.truncate redo_log_name Redo_log.minimum_vdi_size;
  Redo_log.enable_block test_redo_log redo_log_name ;
  let db = Database.register_callback "redo log" Redo_log.database_callback db in
  Redo_log.flush_db_to_redo_log db test_redo_log |> ignore;
  let original = persistent_events_of db f in
  Redo_log.shutdown test_redo_log ;
  let recovered = recover_events test_redo_log (Database.schema db) in
  check_database_events msg original recovered ;

  (* only do this on success, keep it for inspection on failure *)
  Sys.remove redo_log_name

let make_vbd vm r uuid = [
  (*    "ref", r; *)
  "uuid", uuid;
  "VM", vm;
  "type", "user";
]

let () =
  let prog =
    Cmdliner.Arg.(value & opt (some string) None & info ["block-device-io"])
  in
  Debug.log_to_stdout ();
  Alcotest.run_with_args "redo log" prog [
    "dummy", [
      "dummy", `Quick, (fun prog ->
          (match prog with
           | Some prog -> Db_globs.redo_log_block_device_io := prog
           | None -> ());
          let t = Db_backend.make () in
          let db = Parse_db_conf.make "db.xml" in
          Db_conn_store.initialise_db_connections [ db ];
          Db_cache_impl.make t [ db ] Test_schemas.schema;
          Db_cache_impl.sync [ db ] (Db_ref.get_database t);
          let db = Db_ref.get_database t in
          check_database_replay "dummy" ~redo_log_name:"test-db-replay" db (fun db ->
              Db_ref.update_database t (fun _ -> db);
              Db_cache_impl.create_row t "VBD" (make_vbd "vmref" "rrr" "uuid") "vm";
              Db_cache_impl.create_row t "VBD" (make_vbd "vmref2" "rrr" "uuid2") "vm2";
              Db_ref.get_database t
            ))
    ]
  ]
