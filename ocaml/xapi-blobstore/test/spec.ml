open Xapi_blobstore_core

module type KVDirect = sig
  include Types.S with type 'a io = 'a and type config = unit
end

let is_small s = String.length s < 6

let truncated_str s =
  if String.length s < 6 then
    String.escaped s
  else
    Printf.sprintf "%s…(len:%d)"
      (String.sub s 0 6 |> String.escaped)
      (String.length s)

let all_bytes = String.init 256 Char.chr

(** [graft_corners arb corners] adds the static [corners] list of corner cases to [arb]. *)
let graft_corners corners arb =
  let gen = QCheck.get_gen arb in
  let gen = QCheck.Gen.graft_corners gen corners () in
  QCheck.set_gen gen arb

let repeat n = List.init n (fun _ -> all_bytes) |> String.concat ""

let string_64k =
  let b = Buffer.create 2 in
  Array.init 65536 @@ fun i ->
   Buffer.clear b;
   if i < 256 then
     Buffer.add_uint8 b i
   else
     Buffer.add_uint16_ne b i;
   Buffer.contents b
   
let gen_string_64k = QCheck.Gen.oneofa string_64k

let bounded_string_arb =
  (* QCheck.Shrink.string is very slow: first converts to list of chars.
     In our case bugs come from 3 sources:
      * the byte values (e.g. ASCII vs non-ascii)
      * the length of the string (whether it hits various limits or not)
      * the uniqueness of the string (e.g. duplicate inserts)
      
    So map back the string to the integer that generated it and let QCheck shrink an integer,
    which is done efficiently through depth limited binary search.
    
    To further speed up shrinking and generating we pregenerate 256 + 65536 + 2*n strings.
  *)
  let buf = Buffer.create 128 in
  fun f n ->
    let large = Array.init (2*n) @@ fun i ->
      if i mod 2 = 0 then begin
        Buffer.clear buf;
        Buffer.add_int32_ne buf (Int32.of_int i);
        f buf i;
        Buffer.contents buf
      end
      else
        String.make i 'a'        
    in
    let get_large idx = large.(idx) in
    let shrink s =
      let open QCheck in
      if String.length s < 4 then
        Shrink.string s
      else
        let idx = String.get_int32_ne s 0 |> Int32.to_int in
        Iter.map get_large (Shrink.int idx)
    in
    QCheck.(make
      ~print:truncated_str
      ~small:String.length
      ~shrink
      @@ Gen.oneof
       [ gen_string_64k
       ; Gen.oneofa large
       ]
    )
let bounded_string_arb =
  assert (n > 0) ;
  let max_repetitions = n / String.length all_bytes in
  let max_str =
    repeat max_repetitions
    ^ String.sub all_bytes 0 (n mod String.length all_bytes)
  in
  bounded_string_arb @@ fun buf i ->
  Buffer.add_substring buf max_str 0 i

module MakeSTM (KV : KVDirect) : STM.Spec = struct
  let key_to_string' v = v |> KV.Key.to_string |> truncated_str

  let value_to_string' v = v |> KV.Value.to_string |> truncated_str

  let value_equal v1 v2 =
    String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)

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
        Res (result unit exn, protect (KV.put sut k) v)
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

  let kv_length k v =
    let open KV in
    String.length (Key.to_string k) + String.length (Value.to_string v)

  (* TODO: test this on its own that it matches the invariant *)
  module SizedMap = struct
    type t = {map: KV.Value.t KeyMap.t; size: int}

    let empty = {map= KeyMap.empty; size= 0}

    let remove k t =
      match KeyMap.find_opt k t.map with
      | None ->
          t
      | Some old ->
          {map= KeyMap.remove k t.map; size= t.size - kv_length k old}

    let add k v t =
      let t = remove k t in
      {map= KeyMap.add k v t.map; size= t.size + kv_length k v}

    let find_opt k t = KeyMap.find_opt k t.map

    let mem k t = KeyMap.mem k t.map

    let is_empty t = KeyMap.is_empty t.map

    let size t = t.size

    let cardinal t = KeyMap.cardinal t.map

    let to_seq t = KeyMap.to_seq t.map
  end

  type state = int * SizedMap.t

  let () =
    (* the backend must be able to store something,
       this implements the 2 'ensures' constraints from Types.mli *)
    assert (KV.max_data > 0) ;
    assert (KV.max_keys > 0)

  let invariant (_, t) =
    (* implements the invariants on type t *)
    SizedMap.cardinal t <= KV.max_keys && SizedMap.size t <= KV.max_data

  let init_state = 0, SizedMap.empty

  let next_state cmd (i, state) =
    i+1, match cmd with
    | Get _ | List ->
        state (* queries do not change state *)
    | Put (k, v) ->
        SizedMap.add k v state
    | Delete k ->
        SizedMap.remove k state

  let precond _cmd (i, _state) = i <= 8 (* small number of commands due to potential exponential blowup *)

  let postcond cmd ((i, state) : state) res =
    match (cmd, res) with
    | Get k, Res ((Option Value, _), v) ->
        Option.equal value_equal v (SizedMap.find_opt k state)
    | (Put (_, _) as cmd), Res ((Result (Unit, Exn), _), Ok ()) ->
        (* postcond gets the previous state passed in *)
        invariant (next_state cmd (i, state))
    | (Delete _ as cmd), Res ((Unit, _), ()) ->
        (* postcond gets the previous state passed in *)
        invariant (next_state cmd (i, state))
    | ( (Put (_, _) as cmd)
      , Res ((Result (Unit, Exn), _), Error (Invalid_argument _)) ) ->
        not @@ invariant (next_state cmd (i, state))
    | List, Res ((List Key, _), l) ->
        List.length l = SizedMap.cardinal state
        && List.for_all (fun k -> SizedMap.mem k state) l
    | _ ->
        false

  let shrink_cmd =
    let open QCheck in
    let open KV in
    function
    | Get k ->
        Iter.map
          (fun k -> Get (Key.of_string_exn k))
          Shrink.(filter is_small string @@ Key.to_string k)
    | Put (k, v) ->
        Iter.map2
          (fun k v -> Put (Key.of_string_exn k, Value.of_string_exn v))
          Shrink.(filter is_small string @@ Key.to_string k)
          Shrink.(filter is_small string @@ Value.to_string v)
    | Delete k ->
        Iter.map
          (fun k -> Delete (Key.of_string_exn k))
          Shrink.(filter is_small string @@ Key.to_string k)
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

  let arb_cmd (_, state) =
    let open QCheck in
    let gen_new_key =
      bounded_string_arb KV.Key.max_length
      |> QCheck.gen
      |> Gen.map KV.Key.of_string_exn
    in
    let value =
      bounded_string_arb KV.Value.max_length
      |> QCheck.gen
      |> Gen.map KV.Value.of_string_exn
    in
    let key =
      if SizedMap.is_empty state then
        gen_new_key
      else (* TODO: cache for faster access *)
        let keys = state |> SizedMap.to_seq |> Seq.map fst |> Array.of_seq in
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
         ; Gen.map2 (fun k v -> Put (k, v)) key value
         ]
end

module MakeLin (KV : KVDirect) : Lin.Spec = struct
  open Lin

  type t = KV.t

  let init = KV.connect

  let cleanup = KV.disconnect

  let key_to_string' v = v |> KV.Key.to_string |> truncated_str

  let value_to_string' v = v |> KV.Value.to_string |> truncated_str

  let value_equal v1 v2 =
    String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)

  let key_equal v1 v2 = String.equal (KV.Key.to_string v1) (KV.Key.to_string v2)

  let api =
    let max_len = 1024 in (* might take exponential time otherwise... but it also doesn't catch bugs if too short *)
    let key_arb =
      QCheck.map ~rev:KV.Key.to_string KV.Key.of_string_exn
      @@ bounded_string_arb max_len
      (*  KV.Key.max_length *)
    in
    let value_arb =
      QCheck.map ~rev:KV.Value.to_string KV.Value.of_string_exn
      @@ bounded_string_arb KV.Value.max_length
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
module MakeDirect
    (KV : Types.S with type 'a io = 'a Lwt.t and type config = unit) :
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
  , (* KV_seq.agree_test ~count ~name:(KV.name ^ " STM sequential") :: *)
     Conc.tests ~count ~name:KV.name
  )
