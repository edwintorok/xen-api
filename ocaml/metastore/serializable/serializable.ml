type 'a typ = 'a Rpc.Types.typ

module type S = sig
  type t

  val typ_of : t Rpc.Types.typ
end

(* if we already have a way to convert a type to Rpc.t we can use that to dump
   it, but in a readable way, not by using Rpc.to_string *)

let iter_alist f = List.iter @@ fun (k, v) -> f k v

let rec dump_rpc ppf =
  let open Fmt in
  let open Fmt.Dump in
  let open Rpc in
  function
  | Int i64 ->
      int64 ppf i64
  | Int32 i32 ->
      int32 ppf i32
  | Bool b ->
      bool ppf b
  | Float f ->
      float ppf f
  | String s ->
      string ppf s
  | DateTime s ->
      string ppf s
  | Enum lst ->
      list dump_rpc ppf lst
  | Dict alist ->
      iter_bindings iter_alist Fmt.nop string dump_rpc ppf alist
  | Base64 b64 ->
      Fmt.pf ppf "base64:%s" b64
  | Null ->
      string ppf "null"

let dump typ_of t = Fmt.using (Rpcmarshal.marshal typ_of) dump_rpc t

module T = struct
  type 'a t = 'a typ * 'a

  let v typ t = typ, t

  let dump ppf (typ_of, t) = dump typ_of ppf t
end

let using ~aname to_other from_other typ_of_other =
  let open Rpc.Types in
  let rpc_of v = v |> to_other |> Rpcmarshal.marshal typ_of_other
  and of_rpc rpc =
    rpc |> Rpcmarshal.unmarshal typ_of_other |> Result.map from_other
  in
  Abstract
    {
      aname
    ; test_data= Rpc_genfake.gentest typ_of_other |> List.map from_other
    ; rpc_of
    ; of_rpc
    }

let serialize typ_of t = t |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string

let deserialize typ_of str =
  str
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal typ_of
  |> Rresult.R.open_error_msg

module StringMap = Map.Make (String)

type 'a dict = (string * 'a) list [@@deriving rpcty]

let stringmap_of_alist alist = alist |> List.to_seq |> StringMap.of_seq

let typ_of_stringmap typ_of_el =
  using ~aname:"stringmap" StringMap.bindings stringmap_of_alist
  @@ typ_of_dict typ_of_el

let string_of_file =
  Rresult.R.trap_exn @@ fun path ->
  path |> Fpath.to_string |> Xapi_stdext_unix.Unixext.string_of_file

let of_file typ_of path =
  Result.bind (string_of_file path) @@ deserialize typ_of

let string_to_file path =
  Rresult.R.trap_exn
  @@ Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0600
       (Fpath.to_string path)

let to_file typ_of path t = t |> serialize typ_of |> string_to_file path
