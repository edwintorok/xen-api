open Pooled_service_manager
open Xapi_etcd

module MemberConfig = struct
  module Global = struct
    type t = Config.global Config.Section.t
    let to_dict t =
      (* TODO: make this return the proper type in the first place *)
      t |> Config.Section.to_dict Config.Section.global
      |> List.to_seq |> Astring.String.Map.of_seq

    let of_dict kv =
      kv |> Astring.String.Map.bindings
      |> Config.Section.of_dict Config.Section.global
      |> Rresult.R.open_error_msg

  end
  module Local = struct
    type t =  Config.local Config.Section.t

    let to_dict t =
      (* TODO: make this return the proper type in the first place *)
      t |> Config.Section.to_dict Config.Section.local
      |> List.to_seq |> Astring.String.Map.of_seq

    let of_dict kv =
      kv |> Astring.String.Map.bindings
      |> Config.Section.of_dict Config.Section.local
      |> Rresult.R.open_error_msg
  end
  type t = Global.t * Local.t
end


(* TODO: move into etcd config *)
module Etcd = struct
  module Id = struct
    include Uuidm
    let dump = pp
    let to_string t = Uuidm.to_string t
  end
  module Task = struct
    (* only for testing purposes, normally it'd use a thread pool or Async xapi
     calls*)
    type 'a t = { mutable thread: Thread.t option; r: ('a, Rresult.R.exn_trap) result option ref }

    let task f =
      let r = ref None in
      let thread = Thread.create
      (fun () ->
        r := Some (Rresult.R.trap_exn f ())
      )
      () in
      { thread = Some thread; r}

    let await_exn t =
      let () = match t.thread with
      | Some thread ->
          Thread.join thread;
          t.thread <- None
      | None -> ()
      in
      Option.get !(t.r)
      |> function
      | Ok r -> r
      | Error (`Exn_trap (exn, bt)) ->
        Printexc.raise_with_backtrace exn bt
  end
  module Config = struct
    type t = Xapi_etcd.Config.t
    let to_dict = failwith "TODO"
    let of_dict = failwith "TODO" (* TODO: use rpc type here *)
    let equal = failwith "TODO"
    let dump = failwith "TODO"

    let build_config (global, local) =
      let live = failwith "TODO" in
      Xapi_etcd.Config.{ global; local; live}
  end
  module ValidConfig = struct
    type t = string
    let of_config t = Ok (Xapi_etcd.Config.to_environment_file t)
    let dump = failwith "TODO"
    let equal = failwith "TODO"
  end

  let todo _ = failwith "TODO"
  let is_running_exn id = todo id
  let start_exn id _config = todo id
  let stop_exn id _config = todo id
  let reload_exn id ~current:_ ~next:_ = todo id
  let health_check_exn id = todo id
end

module Local = MakeLocal(MemberConfig)(Etcd)(Etcd.Config)
