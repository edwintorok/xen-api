open Bechamel

let test name allocate execute =
  let test_mutex m = execute m ignore in
  Test.make_with_resource ~name
    Test.multiple (* TODO: Test.uniq segfaults here, bechamel bug *)
    ~allocate ~free:ignore (Staged.stage test_mutex)

let () = Suite_init.harness_init ()

let __context = Test_common.make_test_database ()

let observer =
  Xapi_observer.create ~__context ~name_label:"test" ~name_description:""
    ~hosts:[] ~attributes:[] ~endpoints:["bugtool"] ~components:["xapi"]
    ~enabled:true

let host = !Xapi_globs.localhost_ref

let () = Xapi_observer.register ~__context ~self:observer ~host

let mutex_lock_unlock m f = Mutex.lock m ; f () ; Mutex.unlock m

let benchmarks =
  let open Locking_helpers in
  let named_execute m f = Named_mutex.execute m f in
  let named_trace_execute m f =
    Context.with_tracing __context "bench" @@ fun __context ->
    Named_mutex.execute m f
  in
  Test.make_grouped ~name:"Mutex"
    [
      test "Mutex.lock/unlock" Mutex.create mutex_lock_unlock
    ; test "Mutex.execute" Mutex.create
        Xapi_stdext_threads.Threadext.Mutex.execute
    ; test "NamedMutex.execute"
        (fun () -> Named_mutex.create "test")
        named_execute
    ; test "NamedMutex.execute (tracing)"
        (fun () -> Named_mutex.create "test")
        named_trace_execute
    ]

let () =
  Gc.compact () ;
  Memtrace.trace_if_requested () ;
  Bechamel_simple_cli.cli benchmarks
