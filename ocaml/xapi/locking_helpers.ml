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

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module Thread_local_storage = struct
  (* While a thread is alive we keep some per-thread data,
     after the thread dies the data will be GC-ed.
     Ephemerons would allocate some internal options on each lookup,
     so we cannot use them here. Instead we add a finaliser on the Thread.t.
  *)
  type 'a t = {tbl: 'a Thread_table.t; all: IntSet.t Atomic.t; init: unit -> 'a}

  let rec atomic_update t f v =
    let current = Atomic.get t.all in
    let next = f v current in
    if not (Atomic.compare_and_set t.all current next) then (
      (* race, try again, but don't monopolize the CPU *)
      Thread.yield () ;
      atomic_update t f v
    )

  let on_thread_gc t thread =
    let id = Thread.id thread in
    atomic_update t IntSet.remove id ;
    Thread_table.remove t.tbl id

  let find_or_create_unlocked t self =
    (* try/with avoids allocation on fast-path *)
    let id = Thread.id self in
    try Thread_table.find t.tbl id
    with Not_found ->
      (* slow-path: first time use on current thread *)
      (* since we are adding data specific to the current thread,
          and all thread ids are unique we cannot race with another thread here.
         (we may race with other thrads initializing themselves, but that is fine,
          Thread_table handles that)
      *)
      let v = t.init () in
      atomic_update t IntSet.add id ;
      Gc.finalise (on_thread_gc t) self ;
      Thread_table.add t.tbl id v ;
      v

  let get t =
    let self = Thread.self () in
    find_or_create_unlocked t self

  let make init : 'a t =
    let tbl = Thread_table.create () in
    let t = {tbl; all= Atomic.make IntSet.empty; init} in
    (* preallocate storage for current thread *)
    let (_ : 'a) = get t in
    t

  let set_unlocked t v =
    let id = Thread.id (Thread.self ()) in
    Thread_table.remove t.tbl id ;
    Thread_table.add t.tbl id v

  let _set t v = set_unlocked t v

  let snapshot t =
    let all = Atomic.get t.all in
    IntSet.fold
      (fun id map ->
        try IntMap.add id (Thread_table.find t.tbl id) map
        with Not_found ->
          map (* race condition: thread exited and not in table anymore *)
      )
      all IntMap.empty

  let count t = Thread_table.length t.tbl
end

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Make (struct let name = "locking_helpers" end)

open D

type resource_kind = No_resource | Lock of string | Process of string * int

let string_of_resource_kind = function
  | No_resource ->
      ""
  | Lock x ->
      Printf.sprintf "Lock(%s)" x
  | Process (name, pid) ->
      Printf.sprintf "Process(%s, %d)" name pid

let kill_resource = function
  | No_resource ->
      ()
  | Lock x ->
      debug "There is no way to forcibly remove Lock(%s)" x
  | Process (name, pid) ->
      info "Sending SIGKILL to %s pid %d" name pid ;
      Unix.kill pid Sys.sigkill

type resource = {
    kind: resource_kind
  ; str: string
  ; waiting_str: string
  ; acquired_str: string
}

let none = {kind= No_resource; str= ""; waiting_str= ""; acquired_str= ""}

let make kind =
  let str = string_of_resource_kind kind in
  let name state = String.concat "" ["Thread_state."; state; "("; str; ")"] in
  {kind; str; waiting_str= name "waiting_for"; acquired_str= "acquired"}

let lock name = make (Lock name)

let process (name, pid) = make (Process (name, pid))

let kill_resource r = kill_resource r.kind

let is_process name = function
  | {kind= Process (p, _); _} ->
      p = name
  | {kind= No_resource | Lock _; _} ->
      false

let string_of_resource r = r.str

module Thread_state = struct
  type time = float

  type t = {
      mutable last_acquired_resource: resource
    ; mutable last_acquired_at: time
    ; mutable acquired_resources_other: (resource * time) list
    ; mutable task: API.ref_task
    ; mutable name: string
    ; mutable waiting_for: resource
    ; mutable parent: Tracing.Span.t option
    ; mutable span: Tracing.Span.t option
  }

  type waiting = t

  type acquired = t

  let acquired_resources t =
    if t.last_acquired_resource.kind = No_resource then
      t.acquired_resources_other
    else
      (t.last_acquired_resource, t.last_acquired_at)
      :: t.acquired_resources_other

  let make_empty () =
    {
      acquired_resources_other= []
    ; last_acquired_resource= none
    ; last_acquired_at= Float.nan
    ; task= Ref.null
    ; name= ""
    ; waiting_for= none
    ; parent= None
    ; span= None
    }

  let thread_states = Thread_local_storage.make make_empty

  (* to be able to debug locking problems we need a consistent snapshot:
     if we're waiting for a lock, who's holding it currently and what locks are they holding or waiting for?
  *)

  let get_acquired_resources_by_task task =
    let snapshot = Thread_local_storage.snapshot thread_states in
    let all, _ = IntMap.partition (fun _ ts -> ts.task = task) snapshot in
    List.map fst
      (IntMap.fold (fun _ ts acc -> acquired_resources ts @ acc) all [])

  let get_all_acquired_resources () =
    let snapshot = Thread_local_storage.snapshot thread_states in
    List.map fst
      (IntMap.fold (fun _ ts acc -> acquired_resources ts @ acc) snapshot [])

  let get_states () = Thread_local_storage.get thread_states

  let with_named_thread name task f =
    let ts = get_states () in
    ts.name <- name ;
    ts.task <- task ;
    finally f (fun () ->
        let ts = get_states () in
        ts.name <- "" ;
        ts.task <- Ref.null
    )

  let now () = Unix.gettimeofday ()

  let waiting_for ?parent resource =
    let ts = get_states () in
    let () =
      match (parent : Tracing.Span.t option) with
      | None ->
          ()
      | Some _ -> (
          let name = resource.waiting_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent () with
          | Ok span ->
              ts.parent <- parent ;
              ts.span <- span
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e)
        )
    in
    ts.waiting_for <- resource ;
    ts

  let acquired resource ts =
    let () =
      match ts.parent with
      | None ->
          ()
      | Some _ -> (
          let (_ : Tracing.Span.t option) = Tracing.Tracer.finish ts.span in
          let name = resource.acquired_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent:ts.parent () with
          | Ok span ->
              ts.span <- span
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
              ts.span <- None
        )
    in
    ts.waiting_for <- none ;
    if ts.last_acquired_resource.kind <> No_resource then
      ts.acquired_resources_other <-
        (ts.last_acquired_resource, ts.last_acquired_at)
        :: ts.acquired_resources_other ;
    ts.last_acquired_resource <- resource ;
    ts.last_acquired_at <- now () ;
    ts

  let released resource ts =
    let (_ : Tracing.Span.t option) = Tracing.Tracer.finish ts.span in
    if ts.last_acquired_resource == resource then
      ts.last_acquired_resource <- none
    else
      ts.acquired_resources_other <-
        List.filter (fun (r, _) -> r <> resource) ts.acquired_resources_other

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
               (acquired_resources ts)
        )
        snapshot
    in
    let resources_of_ts ts =
      List.map fst (acquired_resources ts)
      @ if ts.waiting_for.kind = No_resource then [] else [ts.waiting_for]
    in
    let all_resources =
      Xapi_stdext_std.Listext.List.setify
        (IntMap.fold (fun _ ts acc -> resources_of_ts ts @ acc) snapshot [])
    in
    let resources_to_ids =
      List.combine all_resources (List.init (List.length all_resources) Fun.id)
    in
    let resources_to_sll =
      List.filter_map
        (function
          | {kind= No_resource; _} ->
              None
          | {kind= Lock x; _} as y ->
              Some (y, [["lock"]; [x]])
          | {kind= Process (name, pid); _} as y ->
              Some (y, [["process"]; [name]; [string_of_int pid]])
          )
        all_resources
    in
    let resources_to_threads =
      IntMap.fold
        (fun id ts acc ->
          List.map
            (fun (r, _) -> (id, List.assoc r resources_to_ids))
            (acquired_resources ts)
          @ acc
        )
        snapshot []
    in
    let threads_to_resources =
      IntMap.fold
        (fun id ts acc ->
          match ts.waiting_for with
          | {kind= No_resource; _} ->
              acc
          | r ->
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

module Named_semaphore = struct
  type t = {
      name: string
    ; sem: Semaphore.Counting.t
    ; r: resource
    ; acquire: t -> Tracing.Span.t option -> Thread_state.acquired
    ; release: t -> Thread_state.acquired -> unit
    ; mutable max: int
    ; max_lock: Mutex.t
  }

  let create ?(max=1) name =
    let acquire t parent =
      let waiting = Thread_state.waiting_for ?parent t.r in
      Semaphore.Counting.acquire t.sem ;
      Thread_state.acquired t.r waiting
    in
    let release t waiting =
      Semaphore.Counting.release t.sem ;
      Thread_state.released t.r waiting
    in
    D.debug "Semaphore %s initially has %d resources" name max ;
    assert (max > 0) ;
    {
      name
    ; sem= Semaphore.Counting.make max
    ; r= lock name
    ; acquire
    ; release
    ; max
    ; max_lock= Mutex.create ()
    }

  let execute ?__context ?parent (x : t) f =
    let parent =
      match parent with
      | None ->
          Option.bind __context Context.tracing_of
      | Some _ as p ->
          p
    in
    Thread_state.with_resource x x.acquire f x.release parent

  let set_max t n =
    if n < 1 then
      Fmt.invalid_arg
        "The semaphore '%s' must have at least 1 resource available, \
         requested: %d"
        t.name n ;
    (* ensure only 1 thread attempts to modify the maximum at a time, this is a slow path *)
    Mutex.lock t.max_lock ;
    D.debug
      "Setting semaphore '%s' to have at most %d resource (current max: %d)"
      t.name n t.max ;

    (* requested to decrease maximum, this might block *)
    while t.max > n do
      if not (Semaphore.Counting.try_acquire t.sem) then (
        D.debug
          "Semaphore '%s' has >%d resources in use, waiting until some of them \
           are released"
          t.name n ;
        (* may block *)
        Semaphore.Counting.acquire t.sem
      ) ;
      t.max <- t.max - 1
    done ;

    (* requested to increase maximum, this doesn't block *)
    while t.max < n do
      (* doesn't block, semaphores can also be acquired and released from any thread *)
      Semaphore.Counting.release t.sem ;
      t.max <- t.max + 1
    done ;
    Mutex.unlock t.max_lock ;
    D.debug "Semaphore '%s' updated to have %d resources (%d in use)" t.name n
      (Semaphore.Counting.get_value t.sem)
end
