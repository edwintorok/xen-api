open Xapi_blobstore_core
open Xen_api_lwt_unix
open Lwt.Syntax

type +'a io = 'a Lwt.t

type t = {
    session_id: API.ref_session
  ; rpc: Rpc.call -> Rpc.response Lwt.t
  ; vm: API.ref_VM
  ; target: Uri.t
}

(* TODO: on cohttp 6.x we can also use a Cohttp_lwt_unix.Connection_cache.t here *)

let target = "https://10.71.56.129"

let pwd = ""

let name = __MODULE__

let version = "0.1" (* TODO: from dune-build-info *)

let call t f = f ~rpc:t.rpc ~session_id:t.session_id

let connect () =
  let rpc = make_json ~timeout:5.0 target in
  let* session_id =
    Session.login_with_password ~rpc ~uname:"root" ~pwd ~version
      ~originator:name
  in
  let t = {rpc; session_id; vm= Ref.null; target= Uri.empty} in
  (* TODO: connect to a particular VM *)
  let* pools = call t Pool.get_all in
  let* host = call t Pool.get_master ~self:(List.hd pools) in
  let+ vm = call t Host.get_control_domain ~self:host in
  {rpc; session_id; vm; target= Uri.of_string target}

let disconnect t = Session.logout ~rpc:t.rpc ~session_id:t.session_id

let blob_uri t (blob_ref : API.ref_blob) =
  let uri = Uri.with_path t.target "get_blob" in
  Uri.with_query' uri [("ref", Ref.string_of blob_ref)]

let list t =
  let+ blobs = call t @@ VM.get_blobs ~self:t.vm in
  List.of_seq (blobs |> List.to_seq |> Seq.map @@ fun (key, _) -> Types.Key key)

let lookup t (Types.Key k) =
  let* blobs = call t @@ VM.get_blobs ~self:t.vm in
  match List.assoc_opt k blobs with
  | None ->
      Lwt.return_none
  | Some blobref ->
      let* response, body = Cohttp_lwt_unix.Client.get (blob_uri t blobref) in
      let* body = Cohttp_lwt.Body.to_string body in
      if Cohttp.Response.status response = `OK then
        Lwt.return_some (blobref, Types.Value body)
      else (* TODO: log *)
        Lwt.fail_with ("Failed to retrieve blob: " ^ body)

let get t k =
  let+ r = lookup t k in
  Option.map snd r

let delete t k =
  let* r = lookup t k in
  match r with
  | None ->
      Lwt.return_unit
  | Some (blobref, _) ->
      call t @@ Blob.destroy ~self:blobref

let put t k (Types.Value v) =
  let* r = lookup t k in
  let* blobref, _ =
    match r with
    | None ->
        (* first write: create blob *)
        let (Types.Key name) = k in
        (* this can fail with a duplicate key error,
           but we're only meant to have one active writer at a time,
           so if that happens it is a bug...
        *)
        let+ blobref =
          call t
          @@ VM.create_new_blob ~vm:t.vm ~name ~mime_type:"" ~public:false
        in
        (blobref, None)
    | Some (blobref, prev) ->
        (* already exists: update blob.
           Relies on a bugfix to put_blob to make it atomic.
        *)
        Lwt.return (blobref, Some prev)
  in
  let body = Cohttp_lwt.Body.of_string v in
  let* response, body = Cohttp_lwt_unix.Client.put ~body (blob_uri t blobref) in
  let* body = Cohttp_lwt.Body.to_string body in
  if Cohttp.Response.status response = `OK then
    Lwt.return_unit
  else (* TODO: log *)
    Lwt.fail_with ("Failed to put blob: " ^ body)
