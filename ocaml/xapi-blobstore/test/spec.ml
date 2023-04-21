open Xapi_blobstore_core

module type KVDirect = sig
  include Types.S with type 'a io = 'a
end

let value_to_string (Types.Value v) = v

let key_to_string (Types.Key k) = k

let value_to_string' v = v |> value_to_string |> String.escaped

let key_to_string' v = v |> key_to_string |> String.escaped

let value_equal (Types.Value v1) (Types.Value v2) = String.equal v1 v2

let key_equal (Types.Key k1) (Types.Key k2) = String.equal k1 k2

module MakeSTM (KV : KVDirect) : STM.Spec = struct
  open STM

  type _ ty += Key : Types.key ty | Value : Types.value ty

  let value = (Value, value_to_string')

  let key = (Key, key_to_string')

  type sut = KV.t

  let init_sut = KV.connect

  let cleanup = KV.disconnect

  type cmd =
    | Get of Types.key
    | Put of Types.key * Types.value
    | Delete of Types.key
    | List

  let show_cmd = function
    | Get k ->
        Printf.sprintf "GET(%S)" @@ key_to_string k
    | Put (k, v) ->
        Printf.sprintf "PUT(%S, %S)" (key_to_string k) (value_to_string v)
    | Delete k ->
        Printf.sprintf "DELETE(%S)" (key_to_string k)
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
    open Types

    type t = key

    let compare (Key k1) (Key k2) = String.compare k1 k2
  end)

  type state = Types.value KeyMap.t

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
    function
    | Get (Types.Key k) ->
        Iter.map (fun k -> Get (Types.Key k)) (Shrink.string k)
    | Put (Types.Key k, Types.Value v) ->
        Iter.map2
          (fun k v -> Put (Types.Key k, Types.Value v))
          (Shrink.string k) (Shrink.string v)
    | Delete (Types.Key k) ->
        Iter.map (fun k -> Delete (Types.Key k)) (Shrink.string k)
    | List ->
        Iter.empty

  let small_cmd = function
    | Get (Types.Key k) | Delete (Types.Key k) ->
        String.length k
    | Put (Types.Key k, Types.Value v) ->
        String.length k + String.length v
    | List ->
        1

  let arb_cmd state =
    let open QCheck in
    let gen_new_key = Gen.map (fun k -> Types.Key k) Gen.small_string in
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
         ; Gen.map2 (fun k v -> Put (k, Types.Value v)) key Gen.string
         ]
end

module MakeLin (KV : KVDirect) : Lin.Spec = struct
  open Lin

  type t = KV.t

  let init = KV.connect

  let cleanup = KV.disconnect

  let api =
    let key_arb =
      QCheck.(map ~rev:key_to_string (fun k -> Types.Key k) small_string)
    in
    let value_arb =
      QCheck.(map ~rev:value_to_string (fun k -> Types.Value k) string)
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
module MakeDirect (KV : Types.S with type 'a io = 'a Lwt.t) :
  KVDirect with type t = KV.t = struct
  type t = KV.t

  type 'a io = 'a

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
