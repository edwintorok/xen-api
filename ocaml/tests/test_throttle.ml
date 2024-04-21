(*
 * Copyright (C) 2024 Cloud Software Group
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

open Alcotest.V1

(* on Linux we seem to get 1us accuracy or better,
   however be more permissive in the test and accept 0.1ms accuracy *)
let expected_accuracy_ns = 100_000L

(* the cost of Condition.wait is not 0, accept 0.5ms although in practice it will be much lower
  *)
let expected_condition_wait_ns = 500_000L

let test_rusage_thread_resolution () =
  let u0 = Rusage_thread.getrusage_thread_ns () in
  let rec loop () =
    let u = Rusage_thread.getrusage_thread_ns () in
    if u = u0 then
      loop ()
    else
      Int64.sub u u0
  in
  let delta = loop () in
  if delta <= 0L then
    failf ~pos:__POS__ "rusage delta expected to be > 0, got: %Ldns" delta ;
  if delta > 10_000_000L then
    failf ~pos:__POS__ "rusage delta expected to be <10ms, got: %Ldns" delta ;
  Printf.printf "Rusage delta: %Ldns" delta

let busy seconds =
  let counter = Mtime_clock.counter () in
  let s = ref 0 in
  let elapsed () =
    1e-9 *. (Mtime_clock.count counter |> Mtime.Span.to_float_ns)
  in
  while elapsed () < seconds do
    for _ = 1 to 10000 do
      s := Sys.opaque_identity (!s + 1)
    done
  done ;
  let (_ : int) = Sys.opaque_identity !s in
  ()

let test_independent_threads () =
  (* test that busy looping other threads don't affect the resource usage of the current thread *)
  let m = Mutex.create ()
  and c = Condition.create ()
  and complete = Atomic.make 0 in
  let completed () =
    if Atomic.fetch_and_add complete 1 = 1 then
      Condition.signal c
  in
  let t1 = Thread.create (fun () -> busy 0.2 ; completed ()) () in
  let t2 = Thread.create (fun () -> busy 0.3 ; completed ()) () in
  Mutex.lock m ;
  let u0 = Rusage_thread.getrusage_thread_ns () in
  (* to get close to 0 here and yet wait for t1 and t2 to complete use condition variables,
     instead of Thread.join
  *)
  while Atomic.get complete < 2 do
    Condition.wait c m
  done ;
  let u1 = Rusage_thread.getrusage_thread_ns () in
  Mutex.unlock m ;
  Thread.join t1 ;
  Thread.join t2 ;
  let delta = Int64.sub u1 u0 in
  assert (delta >= 0L) ;
  Printf.printf "Rusage delta: %Ldns" delta ;
  if delta > expected_condition_wait_ns then
    failf ~pos:__POS__ "rusage delta expected to be close to 0, got: %Ldns"
      delta

let make_test name f =
  test_case name `Quick @@ fun () ->
  (* set timeout to kill test *)
  let (_ : int) = Unix.alarm 5 in
  let finally () =
    (* cancel timeout *)
    let (_ : int) = Unix.alarm 0 in
    ()
  in
  Fun.protect ~finally f

let test_actual_usage () =
  (* test that we measure actual CPU usage, not sleep/wait time *)
  let u0 = Rusage_thread.getrusage_thread_ns () in
  let counter = Mtime_clock.counter () in
  busy 0.3 ;
  let busy_span_ns = Mtime_clock.count counter |> Mtime.Span.to_uint64_ns in
  Thread.delay 0.2 ;
  let u1 = Rusage_thread.getrusage_thread_ns () in
  let delta_ns = Int64.sub u1 u0 in
  let diff_ns = Int64.sub busy_span_ns delta_ns in
  Printf.printf "Rusage: %Ldns, Busy: %Lu, Diff: %Ldns\n" delta_ns busy_span_ns
    diff_ns ;
  (* we want rusage <= mtime. Other processes might've executed on the CPU,
     and the amount of CPU that this thread got might've been less than the elapsed time.
     But it can't be more, except for rounding errors due to clock accuracy.
  *)
  if diff_ns < Int64.mul (-1L) expected_accuracy_ns then
    failf ~pos:__POS__ "rusage and mtime disagree on busy period: %Ldns" diff_ns ;
  if delta_ns < expected_accuracy_ns then
    failf ~pos:__POS__ "Rusage measurement should be ~0.3s, not near 0: %Ldns"
      delta_ns ;
  if busy_span_ns < expected_accuracy_ns then
    failf ~pos:__POS__ "Busy measurement should be ~0.2s, not near 0: %Ldns"
      busy_span_ns

let measure = Throttle.Rusage.make "test"

let test_actual_usage' () =
  (* test that we measure actual CPU usage, not sleep/wait time.
     Use Rusage.measure_rusage
  *)
  let busy_and_idle () = busy 0.3 ; Thread.delay 0.2 in
  Throttle.Rusage.measure_rusage measure busy_and_idle () ;
  let usage, mtime, count = Throttle.Rusage.sample measure in
  check' int ~msg:"count ok" ~expected:1 ~actual:count ;
  let diff = mtime -. usage in
  Printf.printf "Rusage: %.9fs, Busy: %.9fs, Diff: %.9fs\n" usage mtime diff ;
  (* we want rusage <= mtime. Other processes might've executed on the CPU,
     and the amount of CPU that this thread got might've been less than the elapsed time.
     But it can't be more, except for rounding errors due to clock accuracy.
  *)
  if diff < Int64.to_float expected_accuracy_ns *. -1e-9 then
    failf ~pos:__POS__ "rusage and mtime disagree on busy period: %.9f" diff ;
  if usage < 0.01 then
    failf ~pos:__POS__ "Rusage measurement should be ~0.3s, not near 0: %.9fs"
      usage ;
  if mtime < 0.01 then
    failf ~pos:__POS__ "Busy measurement should be ~0.2s, not near 0: %.9fs"
      mtime

let test_controller =
  (* test with 0 delays so that the controller is entirely responsible for inserted delays *)
  Throttle.Controller.make ~max_cpu_usage:0.2 ~delay_before:0. ~delay_between:0.

let test_limit = Throttle.Limit.make measure test_controller

let (_periodic_thread : Thread.t) =
  ()
  |> Thread.create @@ fun () ->
     Debug.with_thread_associated "periodic scheduler"
       Xapi_periodic_scheduler.loop ()

let test_cpu_limiting () =
  let name = "test update" in
  Xapi_periodic_scheduler.add_to_queue name
    (Xapi_periodic_scheduler.Periodic 0.06) 0.06 (fun () ->
      Throttle.Limit.update test_limit
  ) ;
  let actual_usage = Throttle.Rusage.make "actual" in
  for _ = 1 to 30 do
    Throttle.Rusage.measure_rusage actual_usage
      (Throttle.Limit.with_limit test_limit busy)
      0.025
  done ;
  Xapi_periodic_scheduler.remove_from_queue name ;
  let usage, mtime, count = Throttle.Rusage.sample actual_usage in
  let fraction = usage /. mtime in
  Printf.printf "usage: %.9fs, mtime: %.9fs, count: %u, fraction: %.3f\n" usage
    mtime count fraction ;
  if fraction <= 0.1 || fraction >= 0.3 then
    failf ~pos:__POS__ "CPU usage outside of desired range: %.3f" fraction

let () =
  Debug.log_to_stdout () ;
  (* test timeout *)
  Alcotest.run "throttle"
    [
      ( "rusage_thread"
      , [
          make_test "resolution" test_rusage_thread_resolution
        ; make_test "threads independent" test_independent_threads
        ; make_test "actual CPU usage" test_actual_usage
        ; make_test "actual CPU usage (measure_rusage)" test_actual_usage'
        ; make_test "test CPU usage limiting" test_cpu_limiting
        ]
      )
    ]
