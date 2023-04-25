open Lwt.Syntax
open Xapi_blobstore_core
module MakeLwt(KV: Types.S with type 'a io = 'a Lwt.t)(Conf: sig val make_config: unit -> KV.config
end) = struct

  let test_conn_discon _ () =
    let* t = KV.connect @@ Conf.make_config () in
    KV.disconnect t
    
  let test_conn_disconn_multiple _ () =
    (* TODO: 1000 once we got rate limiting in place  *)
    List.init 10 Fun.id
    |> Lwt_list.iter_p @@ fun _ ->
    test_conn_discon () ()
    
  let test_par_get _ () =
    let* t = KV.connect @@ Conf.make_config () in
    let key = KV.Key.of_string_exn "foo" in
    let* _ = List.init 1000 (fun _ -> key)
    |> Lwt_list.map_p (KV.get t)
    in
    KV.disconnect t
    
  let value = Alcotest.testable
     Fmt.(using KV.Value.to_string string)
     (fun a b -> String.equal (KV.Value.to_string a) (KV.Value.to_string b))
    
  let test_put_get_kv key testval () =
    let* t = KV.connect @@ Conf.make_config () in
    let* actual = KV.get t key in
    Alcotest.(check' (option value) ~expected:None ~actual ~msg:"expect key to be absent");
    let* () = KV.put t key testval in
    let+ actual = KV.get t key in
    Alcotest.(check' (option value) ~expected:(Some testval) ~actual ~msg:"expect key to be present")
    
  let test_put_get _ () =
    let unique = Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_string |> KV.Key.of_string_exn in
    test_put_get_kv unique (KV.Value.of_string_exn "testvalue") ()
  
    
  let simplify s =
    match Astring.String.cut ~sep:"__" s with
    | None -> s
    | Some (_, s) -> s

  let tests =
    let open Alcotest_lwt in
    [ simplify KV.name ^ " (smoke tests)",
     [ test_case "connect/disconnect" `Quick test_conn_discon
     ; test_case "put and get" `Quick test_put_get
     ; test_case "parallel get" `Quick test_par_get
     ; test_case "multiple connect/disconnect" `Quick test_conn_disconn_multiple
    ]]
end

module TestConf = struct
  let make_config () =
    let open Xapi_blobstore_client.Xapiblob in
    { target = Xen_api_lwt_unix.uri_ip_json "10.71.56.129";
      uname = "root";
      pwd = Sys.getenv_opt "TESTPWD" |> Option.value ~default:""; (* TODO: for now *)
      vm = Uuidm.of_string "d1f067aa-bcba-4a83-b2a0-08babc8b4dbe" |> Option.get

     }
end

let tests =
  let module M = MakeLwt(Xapi_blobstore_client.Xapiblob)(TestConf) in
  M.tests

let () =
  Random.self_init ();
  Alcotest_lwt.run "blobstore" tests
  |> Lwt_main.run