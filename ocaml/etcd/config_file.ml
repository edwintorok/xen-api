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
