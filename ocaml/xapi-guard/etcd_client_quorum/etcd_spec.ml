open Etcd_service_types
open Lin

let lwt_of_result t =
  let open Lwt.Infix in
  t >>= function
  | Ok ok -> Lwt.return ok
  | Error _e ->
      Lwt.fail_with "TODO: response error status"

let called = ref 0

(* thread-safe way to execute an Lwt.t *)
let lwt f x =
  incr called;
  Lwt_preemptive.run_in_main (fun () -> f x)

let lwt2 f x y =
  incr called;
  Lwt_preemptive.run_in_main (fun () -> f x y)

let (_ : Thread.t) =
  (* we need a single "main thread" for Lwt, and we cannot use the fuzzer's
     threads for that. *)
  let forever, _ = Lwt.wait () in
  () |> Thread.create @@ fun () -> Lwt_main.run forever

module MakeDirect(B: KVBackendLwt) : KVBackendDirect = struct
  type 'a io = 'a
  type t = B.t
  let name = B.name ^ " (run_in_main)"
  let init = B.init
  let cleanup = lwt B.cleanup
  let range = lwt2 B.range
  let  put = lwt2 B.put
  let delete_range = lwt2 B.delete_range
end

module Make(B: KVBackendDirect) : Spec  = struct
  type t = B.t
  let init = B.init

  (* delete everything... *)
  let cleanup = fun t ->
    (* TODO: check result *)
    let (_:_ result) = B.delete_range t Etcd_rpc_types.{
      key = "\x00";
      range_end = "";
      prev_kv = false
    } in
    B.cleanup t

  let api =
    let open Etcd_rpc_pp in
    let ret pp_response =
      let equal_response = (=) in (* the response types use only primitive types *)
      deconstructible (Fmt.result ~ok:pp_response ~error:Status.pp |> Fmt.to_to_string)
        (Result.equal ~ok:equal_response ~error:Status.equal)
      |> returning
    in
    let range_request_arb =
      let open Etcd_rpc_types in
      let to_rr (key, range_end) =
        default_range_request ~key ~range_end ()
      in
      let of_rr (t:range_request) = t.key, t.range_end
      in
      (* TODO: there are a lot more fields here.. *)
      QCheck.(pair string string |> map ~rev:of_rr to_rr)
    in
    let range_request =
      gen range_request_arb (Fmt.to_to_string pp_range_request)
    in
    let put_request_arb =
      let open Etcd_rpc_types in
      let to_rr (key, value) =
        default_put_request ~key ~value ()
      in
      let of_rr (t:put_request) = t.key, t.value
      in
      (* TODO: there are a lot more fields here.. *)
      QCheck.(pair string string |> map ~rev:of_rr to_rr)
    in
    let put_request =
      gen put_request_arb (Fmt.to_to_string pp_put_request)
    in
    let delete_range_request_arb =
      let open Etcd_rpc_types in
      let to_rr (key, range_end) =
        default_delete_range_request ~key ~range_end ()
      in
      let of_rr (t:delete_range_request) = t.key, t.range_end
      in
      (* TODO: there are a lot more fields here.. *)
      QCheck.(pair string string |> map ~rev:of_rr to_rr)
    in
    let delete_range_request =
      gen delete_range_request_arb (Fmt.to_to_string pp_delete_range_request)
    in
    [ val_ "range" B.range (t @-> range_request @-> ret pp_range_response)
    ; val_ "put" B.put (t @-> put_request @-> ret pp_put_response)
    ; val_ "delete_range" B.delete_range (t @-> delete_range_request @-> ret pp_delete_range_response)
    ]
end
