open Xapi_blobstore_core
open Xen_api_lwt_unix
open Lwt.Syntax

module Key : sig
  include Types.BoundedString

  val to_raw : t -> string
  (** [raw t] is the encoded string *)

  val of_raw : string -> t
  (** [of_raw t] builds [t] from an encoded string *)
end = struct
  type t = string

  let max_length = 1024

  let alphabet = Base64.uri_safe_alphabet

  let of_string_exn s =
    let n = String.length s in
    if n > max_length then
      Fmt.invalid_arg "Key too long: %d > %d" n max_length ;
    Base64.encode_exn ~pad:true ~alphabet s

  let to_string s = Base64.decode_exn ~pad:true ~alphabet s

  let to_raw t = t

  let of_raw t = t
end

module Value = BoundedString.Make (struct let max_length = 128 * 1024 end)

let max_keys = 256

let max_data = 256 * 1024

type +'a io = 'a Lwt.t

type config = {vm: Uuidm.t; target: Uri.t; uname: string; pwd: string}

type t = {cache: Xen_api_lwt_unix.SessionCache.t; uri: Uri.t; vm: API.ref_VM}

(* TODO: on cohttp 6.x we can also use a Cohttp_lwt_unix.Connection_cache.t here *)

let name = __MODULE__

let version = "0.1" (* TODO: from dune-build-info *)

let call t f = Xen_api_lwt_unix.SessionCache.with_session t.cache f

let connect config =
  let cache =
    Xen_api_lwt_unix.SessionCache.create ~target:config.target
      ~uname:config.uname ~pwd:config.pwd ~version ~originator:name ()
  in
  let t = {cache; vm= Ref.null; uri= config.target} in
  let+ vm = call t @@ VM.get_by_uuid ~uuid:(Uuidm.to_string config.vm) in
  {t with vm}

let disconnect t = Xen_api_lwt_unix.SessionCache.destroy t.cache

let with_blob_uri t (blob_ref : API.ref_blob) f =
  Xen_api_lwt_unix.SessionCache.with_session t.cache
  @@ fun ~rpc:_ ~session_id ->
  let uri = Uri.with_path t.uri "/blob" in
  let uri =
    Uri.with_query' uri
      [
        ("ref", Ref.string_of blob_ref); ("session_id", Ref.string_of session_id)
      ]
  in
  f uri

let list t =
  let+ blobs = call t @@ VM.get_blobs ~self:t.vm in
  List.of_seq (blobs |> List.to_seq |> Seq.map @@ fun (key, _) -> Key.of_raw key)

let resolver =
  let rewrite svc uri =
    match (Uri.scheme uri, Uri.host uri) with
    | Some "http+unix", Some socket_path ->
        Lwt.return (`Unix_domain_socket socket_path)
    | _ ->
        Resolver_lwt_unix.system_resolver svc uri
  in
  let service = function
    | "http+unix" ->
        Lwt.return_some Resolver.{name= "http"; port= 0; tls= false}
    | s ->
        Resolver_lwt_unix.system_service s
  in
  Resolver_lwt.init ~service ~rewrites:[("", rewrite)] ()

let ssl_client_verify = Conduit_lwt_unix_ssl.Client.{hostname= false; ip= true}

let ctx =
  let+ ctx = Conduit_lwt_unix.init ~ssl_client_verify () in
  Cohttp_lwt_unix.Client.custom_ctx ~ctx ~resolver ()

let lookup t k =
  let* blobs = call t @@ VM.get_blobs ~self:t.vm in
  match List.assoc_opt (Key.to_raw k) blobs with
  | None ->
      Lwt.return_none
  | Some blobref ->
      let* ctx = ctx in
      (* TODO: this will need a resolver for the special http+unix Uri *)
      let* response, body =
        with_blob_uri t blobref @@ Cohttp_lwt_unix.Client.get ~ctx
      in
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
  let* ctx = ctx in
  let* response, body =
    with_blob_uri t blobref @@ Cohttp_lwt_unix.Client.put ~chunked:false ~ctx ~body
  in
  let* body = Cohttp_lwt.Body.to_string body in
  if Cohttp.Response.status response = `OK then
    Lwt.return_unit
  else (* TODO: log *)
    Lwt.fail_with ("Failed to put blob: " ^ body)
