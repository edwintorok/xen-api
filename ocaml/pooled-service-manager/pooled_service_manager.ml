open Types
open Errors

let ( let* ) = Result.bind

(* TODO: reload safetycheck on global... *)

module MakeLocal
    (Conf : MemberConfig)
    (L : Lifecycle) (B : sig
      val build_config : Conf.t -> L.Config.t
    end) =
struct
  include L

  module Config = struct
    include Conf

    (* of_dict/to_dict only used for serialization and debugging,
       not used for actually starting the service, that'll be L.Config.t *)
    let prefix_global = "G:"

    let prefix_local = "L:"

    let prefix p map =
      map |> Astring.String.Map.to_seq |> Seq.map (fun (k, v) -> (p ^ k, v))

    let unprefix map =
      map
      |> Astring.String.Map.to_seq
      |> Seq.map (fun (k, v) -> (Astring.String.drop ~max:2 k, v))
      |> Astring.String.Map.of_seq

    let to_dict (global, local) =
      let g = global |> Global.to_dict |> prefix prefix_global
      and l = local |> Local.to_dict |> prefix prefix_local in
      Astring.String.Map.of_seq Seq.(append g l)

    let of_dict dict =
      let global, local =
        dict
        |> Astring.String.Map.partition (fun k _ ->
               String.starts_with ~prefix:prefix_global k
           )
      in
      let* g = global |> unprefix |> Global.of_dict in
      let* l = local |> unprefix |> Local.of_dict in
      Ok (g, l)
  end

  module ValidConfig = struct
    include L.ValidConfig

    let of_config member_config =
      member_config |> B.build_config |> L.ValidConfig.of_config
  end
end

module MakeGlobal
    (Conf : MemberConfig)
    (L : Lifecycle with type Config.t = Conf.t) (B : sig
      val build_global_config :
        Conf.Global.t -> Conf.Local.t Map.Make(L.Id).t -> Conf.Global.t
    end) =
struct
  module Task = L.Task
  module Id = struct
    include Set.Make (L.Id)

    let dump = Fmt.Dump.iter iter (Fmt.any "idset") L.Id.dump

    let to_string set =
      set
      |> to_seq
      |> Seq.map L.Id.to_string
      |> List.of_seq
      |> String.concat ","
  end

  module IdMap = Map.Make (L.Id)

  module Config = struct
    type t = Conf.Global.t * Conf.Local.t IdMap.t

    let to_dict = failwith "TODO: replace with rpcty serialization"

    let of_dict = failwith "TODO: replace with rpcty serialization"
  end

  module ValidConfig = struct
    type t = (L.ValidConfig.t, error) result IdMap.t
    (* type for get_state return value where some members may succeed, and
       others fail, and we want to report success/failure at a more granular
       level *)

    let equal =
      IdMap.equal
      @@ Result.equal ~ok:L.ValidConfig.equal ~error:Errors.equal_error

    let dump =
      Fmt.Dump.iter_bindings IdMap.iter Fmt.(any "idmap") L.Id.dump
      @@ Fmt.Dump.result ~ok:L.ValidConfig.dump ~error:pp_error

    let error_to_option _ = function Error e -> Some e | Ok _ -> None

    let result_map f idmap : (_ IdMap.t, _) result =
      let (r : (_, _) result IdMap.t) = idmap |> IdMap.map f in
      r |> IdMap.filter_map error_to_option |> IdMap.choose_opt |> function
      | None ->
          (* no errors *)
          Ok (r |> IdMap.filter_map @@ fun _ -> Result.to_option)
      | Some (_, err) ->
          Error err

    let of_config (global, local_map) =
      (* here we want to report failure if any of the map elements have failed *)
      let global = B.build_global_config global local_map in
      local_map
      |> result_map @@ fun local ->
         L.ValidConfig.of_config (global, local) |> Result.map Result.ok
  end

  module Local = Service.Make (L)

  let map_idset f idset =
    idset |> Id.to_seq |> Seq.map (fun id -> (id, f id)) |> IdMap.of_seq

  let lift_option = function
    | Ok None ->
        None
    | Ok (Some x) ->
        Some (Ok x)
    | Error _ as e ->
        Some e

  let get_state idset : (ValidConfig.t option, [> error]) result =
    (* TODO: get result on outside, option if all up or not *)
    let map =
      idset
      |> Id.to_seq
      |> Seq.filter_map (fun id ->
             id
             |> Local.get_state
             |> lift_option
             |> Option.map @@ fun v -> (id, Result.map snd v)
         )
      |> IdMap.of_seq
    in
    if IdMap.is_empty map then
      Ok None
    else
      Ok (Some map)

  let first_error idmap =
    idmap |> IdMap.filter (fun _ -> Result.is_error) |> IdMap.choose |> snd

  let set_state idset (conf_opt : Config.t option) =
    let lookup_id_config id =
      ( id
      , Option.bind conf_opt @@ fun (global, localmap) ->
        IdMap.find_opt id localmap |> Option.map (fun local -> (global, local))
      )
    in
    (* TODO: use map_idset *)
    idset
    |> Id.to_seq
    |> Seq.map lookup_id_config
    (* TODO: parallel map *)
    |> Seq.map (fun (id, desired_config) -> Local.set_state id desired_config)
    (* set as many as possible even if some fail *)
    |> List.of_seq
    |> List.find_opt Result.is_error
    |> Option.value ~default:(Ok ())

  let health_check idset = idset |> map_idset Local.health_check |> first_error

  (* include Service.Make (struct
       module Id = L.Id
       module IdMap = Map.Make(L.Id)

       let error_to_option _ = function
         | Error e -> Some e
         | Ok _ -> None

       let result_map f idmap : (_ IdMap.t, _) result =
         let (r: (_, _) result IdMap.t) = idmap |> IdMap.map f in
         r |> IdMap.filter_map error_to_option
         |> IdMap.choose_opt
         |> function
         | None -> (* no errors *)
           Ok (r |> IdMap.filter_map @@ fun _ -> Result.to_option)
         | Some (_, err) -> Error err

       module Config = struct
         type t = Conf.Global.t * Conf.Local.t IdMap.t

         let to_dict = failwith "TODO: replace with rpcty serialization"
         let of_dict = failwith "TODO: replace with rpcty serialization"
       end

       module ValidConfig = struct
         type t = Config.t * L.ValidConfig.t IdMap.t

         let of_config ((global, local_map) as conf) =
           let global = B.build_global_config global local_map in
           let* local = local_map |> result_map @@ fun local ->
           L.ValidConfig.of_config (global, local)
           in
           Ok (conf, local)

         let to_config (conf, _) = conf

         let equal (_, v1) (_, v2) =
             IdMap.equal L.ValidConfig.equal v1 v2
       end

       let is_running_exn
     end)
  *)
end
