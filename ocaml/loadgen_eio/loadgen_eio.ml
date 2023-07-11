open Eio.Std

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8000)

let refill flow ~off ~len into =
  try Eio.Flow.single_read flow (Cstruct.of_bigarray ~off ~len into)
  with End_of_file -> 0

let responses = Atomic.make 0

let finished_request ~status_code ~content_length:_ ~headers_size:_ =
  if status_code = 200 then
    Atomic.incr responses

let nconn = 100

let str =
  List.init 999 (fun _ -> "GET / HTTP/1.1\r\nHost:localhost\r\n\r\n")
  |> String.concat ""

let process_connection (flow, parser) =
  Eio.Flow.copy_string str flow ;
  Eio.Flow.shutdown flow `Send ;
  for _ = 1 to 999 do
    Zero_http.Response.read parser finished_request
  done ;
  Eio.Flow.close flow

let process_connection_eio env (flow, _) =
  let response, _body = Cohttp_eio.Client.get env ~conn:flow ~host:"localhost" "/" in
  if Http.Response.status response = `OK then
    Atomic.incr responses

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let connections =
    Array.make nconn ()
    |> Array.to_list
    |> Fiber.List.map @@ fun () ->
       let flow = Eio.Net.connect ~sw env#net addr in
       let fd = Eio_unix.Resource.fd_opt flow |> Option.get in
       Eio_unix.Fd.use_exn "setsockopt" fd (fun socket ->
           Unix.setsockopt socket Unix.TCP_NODELAY true
       ) ;
       let buf = Bigstringaf.create (1024 * 999) in
       let zb =
         Zero_http.Zero_buffer.of_bigstring buf ~off:0
           ~len:(Bigstringaf.length buf)
       in
       let parser = Zero_http.Response.create zb refill flow in
       (flow, parser)
  in
  let t0 = Unix.gettimeofday () in
  Fiber.List.iter process_connection connections ;
  let t1 = Unix.gettimeofday () in
  let r = Atomic.get responses in
  Printf.printf "req/s: %g\n (responses: %d)\n" (float r /. (t1 -. t0)) r
