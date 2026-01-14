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

open Client

module D = Debug.Make (struct let name = "tasks" end)

module TaskSet = Set.Make (struct
  type t = API.ref_task

  let compare = compare
end)

(* Return once none of the tasks have a `pending status. *)
let wait_for_all_inner ~rpc ~session_id ~all_timeout ~tasks ~callback =
  let classes =
    List.map (fun task -> Printf.sprintf "task/%s" (Ref.string_of task)) tasks
  in
  let timeout_span =
    match all_timeout with
    | Some t ->
        Some (t *. 1e9 |> Int64.of_float |> Mtime.Span.of_uint64_ns)
    | None ->
        None
  in
  let timer = Mtime_clock.counter () in
  let timeout = 5.0 in
  let get_new_classes task_set =
    TaskSet.fold
      (fun task l -> Printf.sprintf "task/%s" (Ref.string_of task) :: l)
      task_set []
  in
  let rec wait ~token ~task_set ~completed_task_count ~classes =
    if TaskSet.is_empty task_set then
      true
    else
      match timeout_span with
      | Some span when Mtime.Span.compare (Mtime_clock.count timer) span > 0 ->
          let tasks = TaskSet.elements task_set in
          let tasks_str =
            tasks |> List.map Ref.really_pretty_and_small |> String.concat ","
          in
          D.info "Waiting for tasks timed out on %s" tasks_str ;
          false
      | _ ->
          let open Event_types in
          let event_from_rpc =
            Client.Event.from ~rpc ~session_id ~classes ~token ~timeout
          in
          let event_from = Event_types.event_from_of_rpc event_from_rpc in
          let records =
            List.map Event_helper.record_of_event event_from.events
          in
          (* If any records indicate that a task is no longer pending, remove that task from the set. *)
          let pending_task_set, completed_task_count, classes =
            List.fold_left
              (fun (task_set', completed_task_count, _) record ->
                match record with
                | Event_helper.Task (t, Some t_rec) ->
                    if
                      TaskSet.mem t task_set'
                      && t_rec.API.task_status <> `pending
                    then
                      let new_task_set = TaskSet.remove t task_set' in
                      let completed_task_count = completed_task_count + 1 in

                      (* Call the callback function, wait for new tasks if any *)
                      let tasks_to_add = callback completed_task_count t in
                      let new_task_set =
                        List.fold_left
                          (fun task_set task -> TaskSet.add task task_set)
                          new_task_set tasks_to_add
                      in
                      ( new_task_set
                      , completed_task_count
                      , get_new_classes new_task_set
                      )
                    else
                      (task_set', completed_task_count, classes)
                | _ ->
                    (task_set', completed_task_count, classes)
              )
              (task_set, completed_task_count, classes)
              records
          in
          wait ~token:event_from.Event_types.token ~task_set:pending_task_set
            ~completed_task_count ~classes
  in
  let token = "" in
  let task_set =
    List.fold_left
      (fun task_set' task -> TaskSet.add task task_set')
      TaskSet.empty tasks
  in
  wait ~token ~task_set ~completed_task_count:0 ~classes

let wait_for_all ~rpc ~session_id ~tasks =
  wait_for_all_inner ~rpc ~session_id ~all_timeout:None ~tasks
    ~callback:(fun _ _ -> []
  )
  |> ignore

let wait_for_all_with_callback ~rpc ~session_id ~tasks ~callback =
  wait_for_all_inner ~rpc ~session_id ~all_timeout:None ~tasks ~callback
  |> ignore

let with_tasks_destroy ~rpc ~session_id ~timeout ~tasks =
  let wait_or_cancel () =
    D.info "Waiting for %d tasks, timeout: %.3fs" (List.length tasks) timeout ;
    if
      not
        (wait_for_all_inner ~rpc ~session_id ~all_timeout:(Some timeout) ~tasks
           ~callback:(fun _ _ -> []
         )
        )
    then (
      D.info "Canceling tasks" ;
      List.iter
        (fun task ->
          if Client.Task.get_status ~rpc ~session_id ~self:task = `pending then
            Client.Task.cancel ~rpc ~session_id ~task
        )
        tasks ;
      (* cancel is not immediate, give it a reasonable chance to take effect *)
      wait_for_all_inner ~rpc ~session_id ~all_timeout:(Some 60.) ~tasks
        ~callback:(fun _ _ -> []
      )
      |> ignore ;
      false
    ) else
      true
  in
  let destroy_all () =
    List.iter
      (fun task ->
        (* db gc thread in xapi may delete task from tasks table *)
        D.log_and_ignore_exn (fun () ->
            Client.Task.destroy ~rpc ~session_id ~self:task
        )
      )
      tasks
  in
  Xapi_stdext_pervasives.Pervasiveext.finally wait_or_cancel destroy_all

open Client

let run t ?(batch_size = 32) ~on_task_complete apifns =
  (* default batch size: 2*Dom0 vCPUs *)
  let all_tasks = Queue.create () in
  let results = Hashtbl.create 7 in
  let result_of_task task =
    try on_task_complete t task |> Result.ok
    with e -> Result.Error (e, Printexc.get_raw_backtrace ())
  in
  let on_task_done task =
    Hashtbl.replace results task (result_of_task task) ;
    call t @@ Task.destroy ~self:task
  in
  let finally () =
    all_tasks
    |> Queue.iter @@ fun task ->
       (* Task may have been GCed, or we raced on cancel, so ignore exceptions. *)
       D.log_and_ignore_exn @@ fun () ->
       if List.mem `cancel (call t @@ Task.get_allowed_operations ~self:task)
       then
         call t @@ Task.cancel ~task
  in
  Fun.protect ~finally @@ fun () ->
  let next =
    apifns
    |> List.to_seq
    |> Seq.map (fun f ->
        let task = call t f in
        Queue.push task all_tasks ; task
    )
    |> Seq.to_dispenser
  in

  let callback _ task =
    on_task_done task ;
    match next () with None -> [] | Some task -> [task]
  in
  (* start batch_size tasks *)
  let tasks = next |> Seq.of_dispenser |> Seq.take batch_size |> List.of_seq in
  call t @@ wait_for_all_with_callback ~tasks ~callback ;

  all_tasks
  |> Queue.to_seq
  |> Seq.map @@ fun task ->
     match Hashtbl.find_opt results task with
     | Some r ->
         r
     | None ->
         result_of_task task
