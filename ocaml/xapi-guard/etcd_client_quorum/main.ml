open Lwt.Syntax
module M1 = Make_grpc.Make (Memory_backend)
module M2 = Make_json.Make (Memory_backend)
module B2 = Make_json.Make (Disk_backend)
module E = Make_json.Make (Etcd_backend_grpc)
module EJ = Make_json.Make (Etcd_backend_json)
module M = EJ

let () =
  (* TODO: use the Lwt reporter from xapi-logs *)
  Logs.set_reporter @@ Logs_fmt.reporter () ;
  Logs.set_level ~all:true (Some Logs.Info) ;
  (* Logs.set_level ~all:true (Some Logs.Debug); *)
  (* default would be 500% overhead, 120% here matches the value used for
     [space_overhead], and is what oxenstored uses. *)
  Gc.set {(Gc.get ()) with Gc.max_overhead= 120} ;

  (* the default would be 1000 threads, set a more reasonable limit *)
  Lwt_unix.set_pool_size 16 ;

  Lwt.async_exception_hook :=
    fun e ->
      Logs.err (fun m -> m "Asynchronous exception encountered: %a" Fmt.exn e)

let server =
  let port = 12380 in
  Logs.debug (fun m -> m "About to listen on port %d" port) ;
  let* server = M.listen (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  Logs.info (fun m -> m "Server listening on port %d" port) ;

  let finished, wake_finished = Lwt.wait () in
  let shutdown () =
    Logs.info (fun m -> m "Signal received, shutting down") ;
    let+ () = M.shutdown server in
    Logs.info (fun m -> m "Shutdown complete")
  in
  let on_signal _ =
    Lwt.on_success (shutdown ()) (Lwt.wakeup_later wake_finished)
  in
  let (_ : Lwt_unix.signal_handler_id) =
    Lwt_unix.on_signal Sys.sigterm on_signal
  in
  let (_ : Lwt_unix.signal_handler_id) =
    Lwt_unix.on_signal Sys.sigint on_signal
  in
  finished

let () = Lwt_main.run server
