(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module IntMap = Map.Make (Int)

module Thread_local_storage = struct
  module Thread_key = struct
    type t = Thread.t

    let hash t = Thread.id t

    let equal a b = Thread.id a = Thread.id b
  end

  module WeakSet = Weak.Make (Thread_key)
  module WeakTable = Ephemeron.K1.Make (Thread_key)

  (* While a thread is alive we keep some per-thread data,
     after the thread dies the data will be GC-ed.
     This is exactly what weak Ephemeron hashtables provide,
     they are not thread-safe themselves and require a global mutex
  *)
  type 'a t = {
      lock: Mutex.t
    ; threads: WeakSet.t
    ; tbl: 'a WeakTable.t
    ; init: unit -> 'a
  }

  let make init =
    {
      lock= Mutex.create ()
    ; tbl= WeakTable.create 47
    ; threads= WeakSet.create 47
    ; init
    }

  let with_lock t f arg =
    Mutex.lock t.lock ;
    match f t arg with
    | result ->
        Mutex.unlock t.lock ; result
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Mutex.unlock t.lock ;
        Printexc.raise_with_backtrace e bt

  let find_or_create_unlocked t self =
    (* try/with avoids allocation on fast-path *)
    try WeakTable.find t.tbl self
    with Not_found ->
      (* slow-path: first time use on current thread *)
      WeakSet.add t.threads self ;
      let v = t.init () in
      WeakTable.add t.tbl self v ; v

  let get t =
    let self = Thread.self () in
    with_lock t find_or_create_unlocked self

  let set_unlocked t v =
    let self = Thread.self () in
    WeakTable.replace t.tbl self v

  let set t v = with_lock t set_unlocked v

  let snapshot_unlocked t () =
    WeakSet.fold
      (fun thr acc ->
        match WeakTable.find_opt t.tbl thr with
        | None ->
            acc
        | Some v ->
            IntMap.add (Thread.id thr) v acc
      )
      t.threads IntMap.empty

  let snapshot t = with_lock t snapshot_unlocked ()

  let count_unlocked t () = WeakTable.length t.tbl

  let count t = with_lock t count_unlocked ()
end

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Make (struct let name = "locking_helpers" end)

open D

type resource_kind = Lock of string | Process of (string * int)

let string_of_resource_kind = function
  | Lock x ->
      Printf.sprintf "Lock(%s)" x
  | Process (name, pid) ->
      Printf.sprintf "Process(%s, %d)" name pid

type resource = {
    kind: resource_kind
  ; str: string
  ; waiting_str: string
  ; acquired_str: string
}

let make kind =
  let str = string_of_resource_kind kind in
  let name state = String.concat "" ["Thread_state."; state; "("; str; ")"] in
  {kind; str; waiting_str= name "waiting_for"; acquired_str= "acquired"}

let lock name = make (Lock name)

let process (name, pid) = make (Process (name, pid))

let kill_resource = function
  | Lock x ->
      debug "There is no way to forcibly remove Lock(%s)" x
  | Process (name, pid) ->
      info "Sending SIGKILL to %s pid %d" name pid ;
      Unix.kill pid Sys.sigkill

let kill_resource r = kill_resource r.kind

let is_process name = function
  | {kind= Process (p, _); _} ->
      p = name
  | {kind= Lock _; _} ->
      false

let string_of_resource r = r.str

module Thread_state = struct
  type waiting = (Tracing.Span.t option * Tracing.Span.t option) option

  type acquired = Tracing.Span.t option

  type time = float

  type t = {
      acquired_resources: (resource * time) list
    ; task: API.ref_task
    ; name: string
    ; waiting_for: (resource * time) option
  }

  let empty =
    {acquired_resources= []; task= Ref.null; name= ""; waiting_for= None}

  let make_empty () = empty

  let thread_states = Thread_local_storage.make make_empty

  (* to be able to debug locking problems we need a consistent snapshot:
     if we're waiting for a lock, who's holding it currently and what locks are they holding or waiting for?
  *)

  let get_acquired_resources_by_task task =
    let snapshot = Thread_local_storage.snapshot thread_states in
    let all, _ = IntMap.partition (fun _ ts -> ts.task = task) snapshot in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) all [])

  let get_all_acquired_resources () =
    let snapshot = Thread_local_storage.snapshot thread_states in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) snapshot [])

  let update f =
    let old = Thread_local_storage.get thread_states in
    let ts = f old in
    Thread_local_storage.set thread_states ts

  let with_named_thread name task f =
    update (fun ts -> {ts with name; task}) ;
    finally f (fun () -> update (fun ts -> {ts with name= ""; task= Ref.null}))

  let now () = Unix.gettimeofday ()

  let waiting_for ?parent resource =
    let span =
      match (parent : Tracing.Span.t option) with
      | None ->
          None
      | Some _ -> (
          let name = resource.waiting_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent () with
          | Ok span ->
              Some (parent, span)
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
              None
        )
    in
    update (fun ts -> {ts with waiting_for= Some (resource, now ())}) ;
    span

  let acquired resource parent =
    let span =
      match parent with
      | None ->
          None
      | Some (parent, span) -> (
          let (_ : (_, _) result) = Tracing.Tracer.finish span in
          let name = resource.acquired_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent () with
          | Ok span ->
              span
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
              None
        )
    in
    update (fun ts ->
        {
          ts with
          waiting_for= None
        ; acquired_resources= (resource, now ()) :: ts.acquired_resources
        }
    ) ;
    span

  let released resource span =
    let (_ : (_, _) result) = Tracing.Tracer.finish span in
    update (fun ts ->
        {
          ts with
          acquired_resources=
            List.filter (fun (r, _) -> r <> resource) ts.acquired_resources
        }
    )

  let to_graphviz () =
    let t' = now () in
    let snapshot = Thread_local_storage.snapshot thread_states in
    (* Map from thread ids -> record rows *)
    let threads =
      IntMap.map
        (fun ts ->
          [ts.name]
          :: [Ref.really_pretty_and_small ts.task]
          :: List.map
               (fun (r, t) ->
                 [string_of_resource r; Printf.sprintf "%.0f" (t' -. t)]
               )
               ts.acquired_resources
        )
        snapshot
    in
    let resources_of_ts ts =
      List.map fst ts.acquired_resources
      @ Option.fold ~none:[] ~some:(fun (r, _) -> [r]) ts.waiting_for
    in
    let all_resources =
      Xapi_stdext_std.Listext.List.setify
        (IntMap.fold (fun _ ts acc -> resources_of_ts ts @ acc) snapshot [])
    in
    let resources_to_ids =
      List.combine all_resources (List.init (List.length all_resources) Fun.id)
    in
    let resources_to_sll =
      List.map
        (function
          | {kind= Lock x; _} as y ->
              (y, [["lock"]; [x]])
          | {kind= Process (name, pid); _} as y ->
              (y, [["process"]; [name]; [string_of_int pid]])
          )
        all_resources
    in
    let resources_to_threads =
      IntMap.fold
        (fun id ts acc ->
          List.map
            (fun (r, _) -> (id, List.assoc r resources_to_ids))
            ts.acquired_resources
          @ acc
        )
        snapshot []
    in
    let threads_to_resources =
      IntMap.fold
        (fun id ts acc ->
          match ts.waiting_for with
          | None ->
              acc
          | Some (r, _) ->
              (id, List.assoc r resources_to_ids) :: acc
        )
        snapshot []
    in
    let label_of_sll sll =
      let bar = String.concat " | " in
      bar (List.map (fun sl -> "{" ^ bar sl ^ "}") sll)
    in
    let all =
      ["digraph Resources {"; "node [shape=Mrecord];"]
      @ IntMap.fold
          (fun id sll acc ->
            Printf.sprintf "t%d [label=\"%s\"];" id (label_of_sll sll) :: acc
          )
          threads []
      @ ["node [shape=record];"]
      @ List.map
          (fun (resource, id) ->
            Printf.sprintf "r%d [style=filled label=\"%s\"];" id
              (label_of_sll (List.assoc resource resources_to_sll))
          )
          resources_to_ids
      @ List.map
          (fun (t, r) -> Printf.sprintf "t%d -> r%d" t r)
          threads_to_resources
      @ List.map
          (fun (t, r) -> Printf.sprintf "r%d -> t%d" r t)
          resources_to_threads
      @ [
          "rankdir=LR"
        ; "overlap=false"
        ; "label=\"Threads and resources\""
        ; "fontsize=12"
        ; "}"
        ]
    in
    String.concat "\n" all

  let known_threads () = Thread_local_storage.count thread_states

  let with_resource resource acquire f release arg =
    let acquired = acquire resource arg in
    match f () with
    | r ->
        release resource acquired ; r
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        D.log_and_ignore_exn (fun () -> release resource acquired) ;
        Printexc.raise_with_backtrace e bt
end

module Named_mutex = struct
  type t = {
      name: string
    ; m: Mutex.t
    ; r: resource
    ; acquire: t -> Tracing.Span.t option -> Thread_state.acquired
    ; release: t -> Thread_state.acquired -> unit
  }

  let create name =
    let acquire t parent =
      let waiting = Thread_state.waiting_for ?parent t.r in
      Mutex.lock t.m ;
      Thread_state.acquired t.r waiting
    in
    let release t waiting =
      Mutex.unlock t.m ;
      Thread_state.released t.r waiting
    in
    {name; m= Mutex.create (); r= lock name; acquire; release}

  let execute ?__context ?parent (x : t) f =
    let parent =
      match parent with
      | None ->
          Option.bind __context Context.tracing_of
      | Some _ as p ->
          p
    in
    Thread_state.with_resource x x.acquire f x.release parent
end
