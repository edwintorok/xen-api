open Xapi_blobstore_core
open Xen_api_lwt_unix
open Lwt.Syntax

module Key : sig
  include Types.BoundedString
  val to_raw: t -> string
  (** [raw t] is the encoded string *)
  
  val of_raw: string -> t
  (** [of_raw t] builds [t] from an encoded string *)
end = struct
  type t = string

  let max_length = 1024

  let alphabet = Base64.uri_safe_alphabet
  let of_string_exn s =
    let n = String.length s in
    if n > max_length then
      Fmt.invalid_arg "Key too long: %d > %d" n max_length;
    Base64.encode_exn ~pad:true ~alphabet s
    
  let to_string s = Base64.decode_exn ~pad:true ~alphabet s
  
  let to_raw t = t
  let of_raw t = t
end

module Value = BoundedString.Make(struct let max_length = 128*1024 end)

let max_keys = 256
let max_data = 256*1024

type +'a io = 'a Lwt.t

type config = { vm: Uuidm.t; target: Uri.t; uname: string; pwd: string }

type t = {
    session_id: API.ref_session
  ; rpc: Rpc.call -> Rpc.response Lwt.t
  ; vm: API.ref_VM
  ; conf: config
}

(* TODO: on cohttp 6.x we can also use a Cohttp_lwt_unix.Connection_cache.t here *)

let name = __MODULE__

let version = "0.1" (* TODO: from dune-build-info *)

let call t f = f ~rpc:t.rpc ~session_id:t.session_id

let connect config =
  let rpc = make_json ~timeout:5.0 (Uri.to_string config.target) in
  let* session_id =
    Session.login_with_password ~rpc ~uname:config.uname ~pwd:config.pwd ~version
      ~originator:name
  in
  let t = {rpc; session_id; vm= Ref.null; conf = config } in
  let+ vm = call t VM.get_by_uuid ~uuid:(Uuidm.to_string config.vm) in
  { t with vm }

let disconnect t = Session.logout ~rpc:t.rpc ~session_id:t.session_id

let blob_uri t (blob_ref : API.ref_blob) =
  let uri = Uri.with_path t.target "get_blob" in
  Uri.with_query' uri [("ref", Ref.string_of blob_ref)]

let list t =
  let+ blobs = call t @@ VM.get_blobs ~self:t.vm in
  List.of_seq (blobs |> List.to_seq |> Seq.map @@ fun (key, _) -> Key.of_raw key)

let lookup t k =
  let* blobs = call t @@ VM.get_blobs ~self:t.vm in
  match List.assoc_opt (Key.to_raw k) blobs with
  | None ->
      Lwt.return_none
  | Some blobref ->
      let* response, body = Cohttp_lwt_unix.Client.get (blob_uri t.conf blobref) in
      let* body = Cohttp_lwt.Body.to_string body in
      if Cohttp.Response.status response = `OK then
        Lwt.return_some (blobref, Value.of_string_exn body)
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

let put t k v =
  let* r = lookup t k in
  let* blobref, _ =
    match r with
    | None ->
        (* first write: create blob *)
        let name = Key.to_raw k in
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
  let body = Cohttp_lwt.Body.of_string (Value.to_string v) in
  let* response, body = Cohttp_lwt_unix.Client.put ~body (blob_uri t.conf blobref) in
  let* body = Cohttp_lwt.Body.to_string body in
  if Cohttp.Response.status response = `OK then
    Lwt.return_unit
  else (* TODO: log *)
    Lwt.fail_with ("Failed to put blob: " ^ body)
