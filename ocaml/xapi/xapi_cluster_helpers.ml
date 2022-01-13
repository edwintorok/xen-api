(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let all_cluster_operations = [`add; `remove; `enable; `disable; `destroy]

(** check if [op] can be done while [current_ops] are already in progress.*)
let is_allowed_concurrently ~op ~current_ops =
  (* for now, disallow all concurrent operations *)
  false

let report_concurrent_operations_error ~current_ops ~ref_str =
  let current_ops_str =
    let op_to_str = Record_util.cluster_operation_to_string in
    match current_ops with
    | [] ->
        failwith "No concurrent operation to report"
    | [(_, cop)] ->
        op_to_str cop
    | l ->
        "{" ^ String.concat "," (List.map op_to_str (List.map snd l)) ^ "}"
  in
  Some
    ( Api_errors.other_operation_in_progress
    , ["Cluster." ^ current_ops_str; ref_str]
    )

(** Take an internal Cluster record and a proposed operation. Return None iff the operation
    would be acceptable; otherwise Some (Api_errors.<something>, [list of strings])
    corresponding to the first error found. Checking stops at the first error. *)
let get_operation_error ~__context ~self ~op =
  let cr = Db.Cluster.get_record_internal ~__context ~self in
  let ref_str = Ref.string_of self in
  let current_error = None in
  let check c f = match c with Some e -> Some e | None -> f () in
  let assert_allowed_during_rpu __context = function
    | (`add | `remove | `destroy)
      when Helpers.rolling_upgrade_in_progress ~__context ->
        Some (Api_errors.not_supported_during_upgrade, [])
    | _ ->
        None
  in
  (* if other operations are in progress, check that the new operation is allowed concurrently with them *)
  let current_error =
    check current_error (fun () ->
        let current_ops = cr.Db_actions.cluster_current_operations in
        match current_ops with
        | _ :: _ when not (is_allowed_concurrently ~op ~current_ops) ->
            report_concurrent_operations_error ~current_ops ~ref_str
        | _ ->
            check (assert_allowed_during_rpu __context op) (fun () -> None)
    )
  in
  current_error

let assert_operation_valid ~__context ~self ~op =
  match get_operation_error ~__context ~self ~op with
  | None ->
      ()
  | Some (a, b) ->
      raise (Api_errors.Server_error (a, b))

let update_allowed_operations ~__context ~self =
  let check accu op =
    match get_operation_error ~__context ~self ~op with
    | None ->
        op :: accu
    | _ ->
        accu
  in
  let allowed = List.fold_left check [] all_cluster_operations in
  Db.Cluster.set_allowed_operations ~__context ~self ~value:allowed

(** Add to the cluster's current_operations, call a function and then remove from the
    current operations. Ensure allowed_operations is kept up to date throughout. *)
let with_cluster_operation ~__context ~(self : [`Cluster] API.Ref.t) ~doc ~op
    ?policy f =
  let task_id = Ref.string_of (Context.get_task_id __context) in
  Helpers.retry_with_global_lock ~__context ~doc ?policy (fun () ->
      assert_operation_valid ~__context ~self ~op ;
      Db.Cluster.add_to_current_operations ~__context ~self ~key:task_id
        ~value:op ;
      update_allowed_operations ~__context ~self
  ) ;
  (* Then do the action with the lock released *)
  finally f (* Make sure to clean up at the end *) (fun () ->
      try
        Db.Cluster.remove_from_current_operations ~__context ~self ~key:task_id ;
        update_allowed_operations ~__context ~self ;
        Helpers.Early_wakeup.broadcast
          (Datamodel_common._cluster, Ref.string_of self)
      with _ -> ()
  )

module Pem = struct
  open Helpers
  open Cluster_interface
  module Client = Client.Client

  let tls_verification_is_enabled ~__context =
    match Db.Pool.get_all ~__context with
    | [pool] ->
        Db.Pool.get_tls_verification_enabled ~__context ~self:pool
    | _ ->
        D.warn "Pool is undefined %s" __LOC__ ;
        false

  (** create a TLS configuration for clusterd *)
  let init ~__context ~cn =
    {
      cn
    ; server= Gencertlib.Selfcert.xapi_cluster ~cn ()
    ; trusted= [] (* no cert checking for now *)
    }

  (** obtain cluster configuration from a cluster host *)
  let cc_of_cluster_host ~__context host =
    call_api_functions ~__context @@ fun rpc session_id ->
    Client.Cluster_host.get_cluster_config rpc session_id host
    |> SecretString.json_rpc_of_t
    |> Rpcmarshal.unmarshal cluster_config.Rpc.Types.ty
    |> function
    | Ok x ->
        x
    | Error e ->
        raise
          Api_errors.(
            Server_error (internal_error, ["bad response from cluster host"])
          )

  (** get existing TLS configuration or create a new one *)
  let get_tls_config ~__context self =
    let cn = Db.Cluster.get_uuid ~__context ~self in
    let enabled_hosts =
      Db.Cluster.get_cluster_hosts ~__context ~self
      |> List.filter (fun self -> Db.Cluster_host.get_enabled ~__context ~self)
    in
    match enabled_hosts with
    | [] ->
        init ~__context ~cn
    | h :: _ -> (
        let cc = cc_of_cluster_host ~__context h in
        match cc.tls_config with
        | None ->
            D.debug "No TLS config found, generating" ;
            init ~__context ~cn
        | Some tls_config ->
            tls_config
      )

  let update_tls_config ~__context ~verify self =
    let dbg = Context.string_of_task __context in
    let tls_config =
      match (get_tls_config ~__context self, verify) with
      | tls, false ->
          D.warn "update_tls_config: disabling certificate checking" ;
          {tls with trusted= []}
      | tls, true ->
          D.debug "update_tls_config: enabling certificate checking" ;
          (* TODO: assumes we have a single global cert, not per host *)
          {tls with trusted= [tls.server]}
    in
    let result =
      Cluster_client.LocalClient.upd_config
        (Xapi_clustering.rpc ~__context)
        dbg tls_config
    in
    match Idl.IdM.run @@ Cluster_client.IDL.T.get result with
    | Ok () ->
        D.debug "cluster TLS configuration updated"
    | Error error ->
        D.warn "Error occured when updating cluster TLS configuration" ;
        Xapi_clustering.handle_error error
end
