let my_domid = 0 (* TODO: figure this out *)

exception Channel_setup_failed

let finally f g =
  try
    let result = f () in
    g () ; result
  with e -> g () ; raise e

let ip = ref "127.0.0.1"

let send proxy_socket =
  let to_close = ref [] in
  let to_unlink = ref [] in
  finally
    (fun () ->
      let s_ip = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      to_close := s_ip :: !to_close ;
      Unix.bind s_ip (Unix.ADDR_INET (Unix.inet_addr_of_string !ip, 0)) ;
      Unix.listen s_ip 5 ;
      let port =
        match Unix.getsockname s_ip with
        | Unix.ADDR_INET (_, port) ->
            port
        | _ ->
            assert false
      in
      let s_unix = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      to_close := s_unix :: !to_close ;
      let path = Filename.temp_file "channel" "" in
      to_unlink := path :: !to_unlink ;
      if Sys.file_exists path then Unix.unlink path ;
      Unix.bind s_unix (Unix.ADDR_UNIX path) ;
      Unix.listen s_unix 5 ;
      let token = "token" in
      let protocols =
        let open Xcp_channel_protocol in
        [TCP_proxy (!ip, port); Unix_sendmsg (my_domid, path, token)]
      in
      (* We need to hang onto a copy of the proxy_socket so we can run a proxy
         in a background thread, allowing the caller to close their copy. *)
      let proxy_socket = Unix.dup proxy_socket in
      to_close := proxy_socket :: !to_close ;
      let (_ : Thread.t) =
        Thread.create
          (fun (fds, paths) ->
            (* The thread takes over management of the listening sockets *)
            let to_close = ref fds in
            let to_unlink = ref paths in
            let close fd =
              if List.mem fd !to_close then (
                to_close := List.filter (fun x -> x <> fd) !to_close ;
                Unix.close fd
              )
            in
            finally
              (fun () ->
                let readable, _, _ = Unix.select [s_ip; s_unix] [] [] (-1.0) in
                if List.mem s_unix readable then (
                  let fd, _peer = Unix.accept s_unix in
                  to_close := fd :: !to_close ;
                  let buffer = Bytes.make (String.length token) '\000' in
                  let n = Unix.recv fd buffer 0 (Bytes.length buffer) [] in
                  let token' = Bytes.sub_string buffer 0 n in
                  if token = token' then
                    let (_ : int) =
                      Fd_send_recv.send_fd_substring fd token 0
                        (String.length token) [] proxy_socket
                    in
                    ()
                ) else if List.mem s_ip readable then (
                  let fd, _peer = Unix.accept s_ip in
                  List.iter close !to_close ;
                  to_close := fd :: !to_close ;
                  Xapi_stdext_unix.Unixext.proxy_noclose fd proxy_socket
                ) else
                  assert false
                (* can never happen *)
              )
              (fun () ->
                List.iter close !to_close ;
                List.iter Unix.unlink !to_unlink
              )
          )
          (!to_close, !to_unlink)
      in
      (* Handover of listening sockets successful *)
      to_close := [] ;
      to_unlink := [] ;
      protocols
    )
    (fun () ->
      List.iter Unix.close !to_close ;
      List.iter Unix.unlink !to_unlink
    )

let receive protocols =
  let open Xcp_channel_protocol in
  let weight = function
    | TCP_proxy (_, _) ->
        2
    | Unix_sendmsg (domid, _, _) ->
        if my_domid = domid then 3 else 0
    | V4V_proxy (_, _) ->
        0
  in
  let protocol =
    match List.sort (fun a b -> compare (weight b) (weight a)) protocols with
    | [] ->
        raise Channel_setup_failed
    | best :: _ ->
        if weight best = 0 then raise Channel_setup_failed else best
  in
  match protocol with
  | V4V_proxy (_, _) ->
      assert false (* weight is 0 above *)
  | TCP_proxy (ip, port) -> (
      let unwrapped_ip = Scanf.ksscanf ip (fun _ _ -> ip) "[%s@]" Fun.id in
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string unwrapped_ip, port) in
      let family = Unix.domain_of_sockaddr addr in
      let s = Unix.socket family Unix.SOCK_STREAM 0 in
      try Unix.connect s addr ; s with e -> Unix.close s ; raise e
    )
  | Unix_sendmsg (_, path, token) ->
      let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      finally
        (fun () ->
          Unix.connect s (Unix.ADDR_UNIX path) ;
          let (_ : int) =
            Unix.send_substring s token 0 (String.length token) []
          in
          let buf = Bytes.create (String.length token) in
          let _, _, fd = Fd_send_recv.recv_fd s buf 0 (Bytes.length buf) [] in
          fd
        )
        (fun () -> Unix.close s)
