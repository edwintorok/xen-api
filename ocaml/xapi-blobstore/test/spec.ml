open Xapi_blobstore_core

module type KVDirect = sig
  include Types.S with type 'a io = 'a and type config = unit
end


module MakeSTM (KV : KVDirect) : STM.Spec = struct
  let value_to_string' v = v |> KV.Value.to_string |> String.escaped

  let key_to_string' v = v |> KV.Key.to_string |> String.escaped

  let value_equal v1 v2 = String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)

  open STM

  type _ ty += Key : KV.Key.t ty | Value : KV.Value.t ty

  let value = (Value, value_to_string')

  let key = (Key, key_to_string')

  type sut = KV.t

  let init_sut = KV.connect

  let cleanup = KV.disconnect

  type cmd =
    | Get of KV.Key.t
    | Put of KV.Key.t * KV.Value.t
    | Delete of KV.Key.t
    | List

  let show_cmd = function
    | Get k ->
        Printf.sprintf "GET(%S)" @@ key_to_string' k
    | Put (k, v) ->
        Printf.sprintf "PUT(%S, %S)" (key_to_string' k) (value_to_string' v)
    | Delete k ->
        Printf.sprintf "DELETE(%S)" (key_to_string' k)
    | List ->
        "LIST"

  let run cmd sut =
    match cmd with
    | Get g ->
        Res (option value, KV.get sut g)
    | Put (k, v) ->
        Res (unit, KV.put sut k v)
    | Delete k ->
        Res (unit, KV.delete sut k)
    | List ->
        Res (list key, KV.list sut)

  (* pure model *)

  module KeyMap = Map.Make (struct
    type t = KV.Key.t

    let compare k1 k2 =
      String.compare (KV.Key.to_string k1) (KV.Key.to_string k2)
  end)

  type state = KV.Value.t KeyMap.t

  let init_state = KeyMap.empty

  let next_state cmd state =
    match cmd with
    | Get _ | List ->
        state (* queries do not change state *)
    | Put (k, v) ->
        KeyMap.add k v state
    | Delete k ->
        KeyMap.remove k state

  let precond _cmd state = KeyMap.cardinal state <= 10

  let postcond cmd (state : state) res =
    match (cmd, res) with
    | Get k, Res ((Option Value, _), v) ->
        Option.equal value_equal v (KeyMap.find_opt k state)
    | (Put (_, _) | Delete _), Res ((Unit, _), ()) ->
        true
    | List, Res ((List Key, _), l) ->
        List.length l = KeyMap.cardinal state
        && List.for_all (fun k -> KeyMap.mem k state) l
    | _ ->
        false

  let shrink_cmd =
    let open QCheck in
    let open KV in
    function
    | Get k ->
        Iter.map (fun k -> Get (Key.of_string_exn k)) (Shrink.string @@ Key.to_string k)
    | Put (k, v) ->
        Iter.map2
          (fun k v -> Put (Key.of_string_exn k, Value.of_string_exn v))
          (Shrink.string @@ Key.to_string k) (Shrink.string @@ Value.to_string v)
    | Delete k ->
        Iter.map (fun k -> Delete (Key.of_string_exn k)) (Shrink.string @@ Key.to_string k)
    | List ->
        Iter.empty

  let small_cmd =
    let open KV in
    function
    | Get k | Delete k ->
        String.length (Key.to_string k)
    | Put (k, v) ->
        String.length (Key.to_string k) + String.length (Value.to_string v)
    | List ->
        1

  let arb_cmd state =
    let open QCheck in
    let open KV in
    let gen_new_key = Gen.map Key.of_string_exn Gen.small_string in
    let key =
      if KeyMap.is_empty state then
        gen_new_key
      else (* TODO: cache for faster access *)
        let keys = state |> KeyMap.to_seq |> Seq.map fst |> Array.of_seq in
        (* generate brand new keys or keys from the existing map:
           this ensures that delete actually exercises the code that deletes an existing key,
           with random generation there would be a very low chance of that otherwise
        *)
        Gen.(oneof [oneofa keys; gen_new_key])
    in
    make ~print:show_cmd ~shrink:shrink_cmd ~small:small_cmd
    @@ Gen.oneof
         [
           Gen.return List
         ; Gen.map (fun k -> Get k) key
         ; Gen.map (fun k -> Delete k) key
         ; Gen.map2 (fun k v -> Put (k, Value.of_string_exn v)) key Gen.string
         ]
end

module MakeLin (KV : KVDirect) : Lin.Spec = struct
  open Lin

  type t = KV.t

  let init = KV.connect

  let cleanup = KV.disconnect

  let value_to_string' v = v |> KV.Value.to_string |> String.escaped

  let key_to_string' v = v |> KV.Key.to_string |> String.escaped

  let value_equal v1 v2 = String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)
  let key_equal v1 v2 = String.equal (KV.Key.to_string v1) (KV.Key.to_string v2)

  let api =
    let key_arb =
      QCheck.(map ~rev:KV.Key.to_string KV.Key.of_string_exn small_string)
    in
    let value_arb =
      QCheck.(map ~rev:KV.Value.to_string KV.Value.of_string_exn string)
    in
    (* needs to be (), otherwise the constructible/deconstructible type parameter will be weak,
       instead of forall 'a and won't work both as parameter and return value *)
    let key () = Lin.gen_deconstructible key_arb key_to_string' key_equal in
    let value () =
      Lin.gen_deconstructible value_arb value_to_string' value_equal
    in
    [
      val_ "GET" KV.get (t @-> key () @-> returning @@ option @@ value ())
    ; val_ "PUT" KV.put (t @-> key () @-> value () @-> returning unit)
    ; val_ "DELETE" KV.delete (t @-> key () @-> returning unit)
    ; val_ "LIST" KV.list (t @-> returning @@ list @@ key ())
    ]
end

(* TODO: make the tests one below take an io monad? *)
module MakeDirect (KV : Types.S with type 'a io = 'a Lwt.t and type config = unit) :
  KVDirect with type t = KV.t = struct
  type t = KV.t

  type 'a io = 'a
  type config = KV.config
  let max_keys = KV.max_keys
  let max_data = KV.max_data

  module Key = KV.Key
  module Value = KV.Value
  let name = KV.name ^ " (run_in_main)"

  let lwt f x = Lwt_preemptive.run_in_main (fun () -> f x)

  let lwt2 f x y = Lwt_preemptive.run_in_main (fun () -> f x y)

  let lwt3 f x y z = Lwt_preemptive.run_in_main (fun () -> f x y z)

  let connect = lwt KV.connect

  let disconnect = lwt KV.disconnect

  let get = lwt2 KV.get

  let put = lwt3 KV.put

  let delete = lwt2 KV.delete

  let list = lwt KV.list
end

let tests (module KV : KVDirect) ~count =
  let module SpecSTM = MakeSTM (KV) in
  let module KV_seq = STM_sequential.Make (SpecSTM) in
  let module SpecLin = MakeLin (KV) in
  let module Conc = Spec_concurrent.Make (SpecSTM) (SpecLin) in
  ( KV.name
  , KV_seq.agree_test ~count ~name:(KV.name ^ " STM sequential")
    :: Conc.tests ~count ~name:KV.name
  )
