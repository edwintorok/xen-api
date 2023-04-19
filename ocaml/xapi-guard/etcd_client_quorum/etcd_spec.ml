open Etcd_service_types

let lwt_of_result t =
  let open Lwt.Infix in
  t >>= function
  | Ok ok ->
      Lwt.return ok
  | Error _e ->
      Lwt.fail_with "TODO: response error status"

let called = ref 0

(* thread-safe way to execute an Lwt.t *)
let lwt f x =
  incr called ;
  Lwt_preemptive.run_in_main (fun () -> f x)

let lwt2 f x y =
  incr called ;
  Lwt_preemptive.run_in_main (fun () -> f x y)

let (_ : Thread.t) =
  (* we need a single "main thread" for Lwt, and we cannot use the fuzzer's
     threads for that. *)
  let forever, _ = Lwt.wait () in
  () |> Thread.create @@ fun () -> Lwt_main.run forever

module MakeDirect (B : KVBackendLwt) : KVBackendDirect = struct
  type 'a io = 'a

  type t = B.t

  let name = B.name ^ " (run_in_main)"

  let init = B.init

  let cleanup = lwt B.cleanup

  let range = lwt2 B.range

  let put = lwt2 B.put

  let delete_range = lwt2 B.delete_range
end

let range_request_arb =
  let open Etcd_rpc_types in
  let to_rr (key, range_end) = default_range_request ~key ~range_end () in
  let of_rr (t : range_request) = (t.key, t.range_end) in
  (* TODO: there are a lot more fields here.. *)
  QCheck.(pair small_printable_string small_printable_string |> map ~rev:of_rr to_rr)

let put_request_arb =
  let open Etcd_rpc_types in
  let to_rr (key, value) = default_put_request ~key ~value () in
  let of_rr (t : put_request) = (t.key, t.value) in
  (* TODO: there are a lot more fields here.. *)
  QCheck.(pair small_printable_string small_printable_string |> map ~rev:of_rr to_rr)

let delete_range_request_arb =
  let open Etcd_rpc_types in
  let to_rr (key, range_end) =
    default_delete_range_request ~key ~range_end ()
  in
  let of_rr (t : delete_range_request) = (t.key, t.range_end) in
  (* TODO: there are a lot more fields here.. *)
  QCheck.(pair small_printable_string small_printable_string |> map ~rev:of_rr to_rr)

module Make (B : KVBackendDirect) : Lin.Spec = struct
  open Lin

  type t = B.t

  let init = B.init

  (* delete everything... *)
  let cleanup t =
    (* TODO: check result *)
    let (_ : _ result) =
      B.delete_range t
        Etcd_rpc_types.{key= "\x00"; range_end= ""; prev_kv= false}
    in
    B.cleanup t

  let api =
    let open Etcd_rpc_pp in
    let ret pp_response =
      let equal_response = ( = ) in
      (* the response types use only primitive types *)
      deconstructible
        (Fmt.result ~ok:pp_response ~error:Status.pp |> Fmt.to_to_string)
        (Result.equal ~ok:equal_response ~error:Status.equal)
      |> returning
    in
    let range_request =
      gen range_request_arb (Fmt.to_to_string pp_range_request)
    in
    let put_request = gen put_request_arb (Fmt.to_to_string pp_put_request) in
    let delete_range_request =
      gen delete_range_request_arb (Fmt.to_to_string pp_delete_range_request)
    in
    [
      val_ "range" B.range (t @-> range_request @-> ret pp_range_response)
    ; val_ "put" B.put (t @-> put_request @-> ret pp_put_response)
    ; val_ "delete_range" B.delete_range
        (t @-> delete_range_request @-> ret pp_delete_range_response)
    ]
end

module MakeSTM (B : KVBackendDirect) : STM.Spec = struct
  open STM

  type sut = B.t

  let init_sut = B.init

  let cleanup = B.cleanup

  open Etcd_rpc_types

  type cmd =
    | Range of range_request
    | Put of put_request
    | Delete_range of delete_range_request

  let show_cmd =
    Fmt.to_to_string @@ fun ppf -> function Range r ->
        Fmt.pf ppf "range(%a)" Etcd_rpc_pp.pp_range_request r | Put p ->
        Fmt.pf ppf "put(%a)" Etcd_rpc_pp.pp_put_request p | Delete_range d ->
        Fmt.pf ppf "delete_range(%a)" Etcd_rpc_pp.pp_delete_range_request d

  type 'a ty +=
    | Range_response : range_response ty
    | Put_response : put_response ty
    | Delete_range_response : delete_range_response ty
    | Status : status ty

  let range_response =
    (Range_response, Fmt.to_to_string Etcd_rpc_pp.pp_range_response)

  let put_response = (Put_response, Fmt.to_to_string Etcd_rpc_pp.pp_put_response)

  let delete_range_response =
    ( Delete_range_response
    , Fmt.to_to_string Etcd_rpc_pp.pp_delete_range_response
    )

  let status = (Status, Fmt.to_to_string Status.pp)

  let run cmd sut =
    match cmd with
    | Range r ->
        Res (result range_response status, B.range sut r)
    | Put p ->
        Res (result put_response status, B.put sut p)
    | Delete_range r ->
        Res (result delete_range_response status, B.delete_range sut r)

  module StringMap = Map.Make (String)

  type state = Kv_types.key_value StringMap.t

  let init_state = StringMap.empty

  let next_state cmd state =
    match cmd with
    | Range _ ->
        state (* doesn't modify state *)
    | Put p ->
        (* TODO: support more fields *)
        let v = Kv_types.default_key_value ~key:p.key ~value:p.value () in
        StringMap.add p.key v state
    | Delete_range d ->
        (* TODO: actual range support *)
        StringMap.remove d.key state

  let precond _cmd _state = true
  
  (* TODO: range not just one ! *)
  let find_range state (key) =
    StringMap.find_opt key state |> Option.to_list
    
  let eq_kv (kv1:Kv_types.key_value) (kv2:Kv_types.key_value) =
    String.equal kv1.key kv2.key
    && String.equal kv1.value kv2.value

  let postcond cmd state res =
    match cmd, res with
    | Range r, Res((Result(Range_response, Status), _), Ok rr) ->
      List.equal eq_kv (find_range state r.key) rr.kvs
    | Put p, Res((Result(Put_response, Status), _), Ok pr) ->
      if p.prev_kv then
        Option.equal eq_kv pr.prev_kv @@ StringMap.find_opt p.key state
      else Option.is_none pr.prev_kv
    | Delete_range dr, Res((Result(Delete_range_response, Status), _), Ok drr) ->
      if dr.prev_kv then
        List.equal eq_kv drr.prev_kvs @@ find_range state dr.key
      else drr.prev_kvs = []
    | _ -> false (* TODO: print? *)


  let arb_cmd _state =
    QCheck.make ~print:show_cmd
      (QCheck.(Gen.oneof
         [
           Gen.map (fun r -> Range r) (gen range_request_arb)
         ; Gen.map (fun p -> Put p) (gen put_request_arb)
         ; Gen.map (fun d -> Delete_range d) (gen delete_range_request_arb)
         ]
      ))
end