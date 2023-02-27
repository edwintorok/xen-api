module StringMap = Map.Make (String)

(* we want to raise conversion errors as early as possible, thus store this as
   a string map *)

type t = string StringMap.t

let empty = StringMap.empty

let to_env_key_char = function '-' -> '_' | c -> Char.uppercase_ascii c

let to_env_key k = "ETCD_" ^ (k |> String.map to_env_key_char)

exception FieldError of (string * exn)

let add_exn field value t =
  let key, value = Config_field.to_string_pair_exn field value in
  StringMap.add key value t

let union_exn = StringMap.union @@ fun key v1 v2 ->
  if String.equal v1 v2 then Some v1
  else Fmt.invalid_arg "Conflicting values for field %S: %S and %S" key v1 v2

let is_valid_key =
  (* prevent shell injection *)
  String.for_all @@ function '_' | 'A' .. 'Z' -> true | _ -> false

let to_environment_line (key, value) =
  if not @@ is_valid_key key then
    invalid_arg ("Invalid ETCD configuration key: " ^ key) ;
  Printf.sprintf "%s=%s" (to_env_key key) (Filename.quote value)

let to_environment_file t =
  t
  |> StringMap.to_seq
  |> Seq.map to_environment_line
  |> List.of_seq
  |> String.concat "\n"

let to_dict = StringMap.bindings
let of_dict dict = dict |> List.to_seq |> StringMap.of_seq

let delta ~current ~desired =
  let diff _ current_value desired_value =
    match current_value, desired_value with
    | None, None -> None (* should never happen, but unchanged *)
    | Some v1, Some v2 when String.equal v1 v2 -> None (* key unchanged *)
    | _, (Some _ as v) -> Some v (* (key, Some v) i.e. set to [v] *)
    | Some _, None -> Some None (* (key, None), i.e. delete 'key' *)
  in
  StringMap.merge diff current desired
  |> StringMap.bindings

