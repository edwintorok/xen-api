open Xapi_blobstore_core
open Lwt.Syntax

let clean_name name =
  let prefix = "Dune__exe__" in
  if String.starts_with ~prefix name then
    let prefixlen = String.length prefix in
    String.sub name prefixlen (String.length name - prefixlen)
  else
    name

module Direct2Lwt (D : Types.KVDirect) :
  Types.KVLwt
    with type t = D.t
     and type config = D.config
     and type Key.t = D.Key.t
     and type Value.t = D.Value.t = struct
  module IO = struct type 'a t = 'a Lwt.t end

  module Key = D.Key
  module Value = D.Value

  type t = D.t

  type config = D.config

  let name = D.name ^ " (direct2lwt)"

  let max_data_size = D.max_data_size

  let max_key_count = D.max_key_count

  let pp_config = D.pp_config

  let connect = Lwt.wrap1 D.connect

  let disconnect = Lwt.wrap1 D.disconnect

  let get = Lwt.wrap2 D.get

  let put = Lwt.wrap3 D.put

  let delete = Lwt.wrap2 D.delete

  let list = Lwt.wrap1 D.list
end

module Make (KV : Types.KVLwt) = struct
  open Logs_lwt

  let test_conn_disconn config _ () =
    let config = Lazy.force config in
    let* () = debug (fun m -> m "Connecting to %a" KV.pp_config config) in
    let* t = KV.connect config in

    let* () = debug (fun m -> m "Connected. Disconnecting... ") in
    let* () = KV.disconnect t in
    debug (fun m -> m "disconnected")

  let max_vm_per_host = 1000

  let test_multiple_conn_disconn config _ () =
    let* () =
      info (fun m -> m "Creating %d connections concurrently" max_vm_per_host)
    in
    List.init max_vm_per_host Fun.id
    |> Lwt_list.iter_p @@ fun _ -> test_conn_disconn config () ()

  let test_par_get config _ () =
    let config = Lazy.force config in
    let* t = KV.connect config in
    let key = KV.Key.of_string_exn "foo" in
    let* () =
      info (fun m -> m "Retrieving %d values concurrently" max_vm_per_host)
    in
    let* _ =
      List.init max_vm_per_host (fun _ -> key) |> Lwt_list.map_p (KV.get t)
    in
    KV.disconnect t

  let value =
    Alcotest.testable
      Fmt.(using KV.Value.to_string string)
      (fun a b -> String.equal (KV.Value.to_string a) (KV.Value.to_string b))

  let pp_value = Fmt.(using KV.Value.to_string string)

  let test_put_get_kv config key testval () =
    let config = Lazy.force config in
    let* t = KV.connect config in
    let* actual = KV.get t key in
    let* () =
      debug (fun m ->
          m "GET(%s) = %a" KV.Key.(to_string key) Fmt.(option pp_value) actual
      )
    in
    Alcotest.(
      check' (option value) ~expected:None ~actual
        ~msg:"expect key to be absent"
    ) ;
    let* () = KV.put t key testval in
    let* () =
      debug (fun m -> m "PUT(%s) = %a" KV.Key.(to_string key) pp_value testval)
    in
    let* actual = KV.get t key in
    let+ () =
      debug (fun m ->
          m "GET(%s) = %a" KV.Key.(to_string key) Fmt.(option pp_value) actual
      )
    in
    Alcotest.(
      check' (option value) ~expected:(Some testval) ~actual
        ~msg:"expect key to be present"
    )

  let test_put_get config _ () =
    let unique =
      Uuidm.v4_gen (Random.get_state ()) ()
      |> Uuidm.to_string
      |> KV.Key.of_string_exn
    in
    test_put_get_kv config unique (KV.Value.of_string_exn "testvalue") ()

  let simplify s =
    match Astring.String.cut ~sep:"__" s with None -> s | Some (_, s) -> s

  let test_case name speed f =
    let f switch () =
      (* needed such that logs go to correct output file between tests *)
      Lwt_switch.add_hook (Some switch) Lwt_io.flush_all ;
      f switch ()
    in
    Alcotest_lwt.test_case name speed f

  let tests make_test_config =
    let config = Lazy.from_fun make_test_config in
    ( clean_name KV.name
    , [
        test_case "connect/disconnect" `Quick @@ test_conn_disconn config
      ; test_case "multiple connect/disconnect" `Slow
        @@ test_multiple_conn_disconn config
      ; test_case "put and get" `Quick @@ test_put_get config
      ; test_case "parallel get" `Slow @@ test_par_get config
      ]
    )
end

(** Reporter from https://ocaml.org/p/logs/latest/doc/Logs_lwt/index.html#report_ex.
    Otherwise all log calls would be blocking, which may change timings in unexpected ways during the test.
*)
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b ; m
    )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
            Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ ->
            Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over () ; Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result ;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  {Logs.report}

let make_direct (type config)
    (module M : Types.KVDirect with type config = config)
    (make_test_config : unit -> config) =
  ((module M : Types.KVDirect with type config = config), make_test_config)

let make_lwt (type config) (module M : Types.KVLwt with type config = config)
    (make_test_config : unit -> config) =
  ((module M : Types.KVLwt with type config = config), make_test_config)

let shutdown = Lwt_switch.create ()

(* logout sessions *)
let () = Lwt_main.at_exit (fun () -> Lwt_switch.turn_off shutdown)

let cache =
  Lazy.from_fun @@ fun () ->
  let target = Uri.make ~host:(Sys.getenv "TEST_BOX") ~scheme:"https" () in
  let uname = Sys.getenv "TEST_UNAME" in
  let pwd = Sys.getenv "TEST_PWD" in
  (* TODO: use version from dune configurator *)
  Xen_api_lwt_unix.SessionCache.create_uri ~switch:shutdown ~target ~uname ~pwd
    ~originator:"test" ~version:"0.1" ()

let make_setget_config () =
  (* TODO: look up or create this instead *)
  Xapi_blobstore_setget.
    {
      cache= Lazy.force cache
    ; vtpm_uuid= Sys.getenv "TEST_VTPM" |> Uuidm.of_string |> Option.get
    }
  

let tests =
  let direct_modules = [make_direct (module Safe_table) Fun.id] in
  let lwt_modules =
    [make_lwt (module Xapi_blobstore_setget) make_setget_config]
  in
  let make_smoketest_lwt (type config)
      ((module SUT : Types.KVLwt with type config = config), make_test_config) =
    let module M = Make (SUT) in
    M.tests make_test_config
  in
  let make_smoketest (type config)
      ((module SUT : Types.KVDirect with type config = config), make_test_config)
      =
    make_smoketest_lwt ((module Direct2Lwt (SUT)), make_test_config)
  in
  let make_qtests sut =
    let name, qtests = Spec.tests ~count:10 sut in
    ( clean_name name
    , qtests
      |> List.map @@ fun qtest ->
         let name, speed, f =
           qtest |> QCheck_alcotest.to_alcotest ~verbose:true
         in
         Alcotest_lwt.test_case_sync (clean_name name) speed f
    )
  in
  List.concat
    [
      direct_modules
      |> List.concat_map (fun sut -> [make_smoketest sut; make_qtests sut])
    ; lwt_modules |> List.concat_map (fun sut -> [make_smoketest_lwt sut])
    ]

let () =
  Logs.set_reporter @@ lwt_reporter () ;
  Logs.set_level (Some Logs.Debug) ;
  Lwt_main.run @@ Alcotest_lwt.run Sys.executable_name tests