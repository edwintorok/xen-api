open Bechamel
open Test_internal

let domain_count =
  ( if Sys.ocaml_release.major < 5 then Cpu.numcores ()
    else Domain.recommended_domain_count () )
  - 1


module Domains = struct
  let ok = Some (Ok ())

  type 'a work =
    { m: Mutex.t
    ; c: Condition.t
    ; finished: Condition.t
    ; mutable task: (bool Atomic.t -> 'a -> int -> unit) option
    ; mutable result: (unit, exn * Printexc.raw_backtrace) result option
    ; stop: bool Atomic.t }

  let make stop =
    { m= Mutex.create ()
    ; c= Condition.create ()
    ; finished= Condition.create ()
    ; task= None
    ; result= Some(Ok ()) (* for idle domains to exit correctly with wait_all_work *)
    ; stop }

  let rec get_work_locked t () =
    if Atomic.get t.stop then raise_notrace Thread.Exit ;
    match t.task with
    | None ->
        Condition.wait t.c t.m ; get_work_locked t ()
    | Some task ->
        t.task <- None ;
        task

  let set_work_locked t task =
    t.result <- None ;
    t.task <- Some task

  let with_mutex t f arg =
    Mutex.lock t.m ;
    match f t arg with
    | r ->
        Mutex.unlock t.m ; r
    | exception e ->
        Mutex.unlock t.m ; raise e

  let set_work t task =
    with_mutex t set_work_locked task ;
    Condition.signal t.c

  let get_work t = with_mutex t get_work_locked ()

  let set_result_locked t result = t.result <- result

  let rec worker work data i () =
    let task = get_work work in
    let result =
      try task work.stop data i ; ok
      with e ->
        Atomic.set work.stop true ;
        Some (Error (e, Printexc.get_raw_backtrace ()))
    in
    with_mutex work set_result_locked result ;
    Condition.signal work.finished ;
    (worker [@tailcall]) work data i ()

  let worker work data i () =
    match worker work data i () with
    | () | exception Thread.Exit -> ()

  let rec wait_work_locked work () =
    match work.result with
    | None ->
        Condition.wait work.finished work.m ;
        (wait_work_locked [@tailcall]) work ()
    | Some (Ok ()) ->
        ()
    | Some (Error (e, bt)) ->
        Printexc.raise_with_backtrace e bt

  let wait_all_work work = with_mutex work wait_work_locked ()

  type 'a t =
  { data: 'a
  ; stop: bool Atomic.t
  ; work: 'a work array
  ; domains: unit Domain.t array
  }

  let alloc create () =
    let data = create () and stop = Atomic.make false in
    let work = Array.init domain_count (fun _ -> make stop) in
    { data
    ; stop
    ; work
    ; domains = Array.mapi (fun i w -> Domain.spawn @@ worker w data i) work }

  let run_on t i f =
    let work = t.work.(i) in
    set_work work f

  let wait_all {work; _} = Array.iter wait_all_work work

  let broadcast_stop work =
    Condition.broadcast work.c;
    Condition.broadcast work.finished

  let free {stop; domains;work; _} =
    Atomic.set stop true ;
    Array.iter broadcast_stop work;
    Array.iter Domain.join domains

  let data {data; _} = data
end

let test_fifo ~name ~safe (module F : Types.FIFO) =
  let allocate () =
    let q = F.create () in
    for i = 1 to 1000 do
      F.push q i
    done ;
    q
  in
  let test_push t = F.push t 42 and test_pop t = F.pop_opt t in
  let init () = (F.create (), Atomic.make 0, Atomic.make 0) in
  let push_count = 1000 in
  let n = domain_count / 2 in
  let expected = n * push_count in
  let domains_allocate = Domains.alloc init
  and domains_free = Domains.free
  and run_push stop (q, domains_pushed, _) _i =
    for x = 1 to push_count do
      if not (Atomic.get stop) then (F.push q x ; Atomic.incr domains_pushed)
    done
  in
  let rec run_pop stop ((q, domains_pushed, domains_popped) as t) i =
    if Atomic.get stop then ()
    else if Atomic.get domains_pushed < expected then (
      (* pop as many as possible while not all elements have been pushed *)
      match F.pop_opt q with
      | None ->
          Domain.cpu_relax () ;
          (run_pop [@tailcall]) stop t i
      | Some _ ->
          Atomic.incr domains_popped ;
          (run_pop [@tailcall]) stop t i )
    else
      (* pop remaining, this avoids an infinite loop if there is a bug in the queue,
         and it doesn't pop the correct number of elements *)
      while F.pop_opt q <> None && not (Atomic.get stop) do
        Atomic.incr domains_popped
      done
  in
  let domains_run t =
    let _, domains_pushed, domains_popped = Domains.data t in
    Atomic.set domains_pushed 0 ;
    Atomic.set domains_popped 0 ;
    for i = 0 to n - 1 do
      Domains.run_on t i run_pop
    done ;
    for i = n to (2 * n) - 1 do
      Domains.run_on t i run_push
    done ;
    Domains.wait_all t ;
    let p1, p2 = (Atomic.get domains_pushed, Atomic.get domains_popped) in
    if p1 <> p2 then failwith (Printf.sprintf "pushed:%d <> popped:%d" p1 p2)
  in
  ( ( Test.make_with_resource ~name ~allocate:F.create ~free:ignore
        Test.multiple (Staged.stage test_push)
    , Test.make_with_resource ~name ~allocate ~free:ignore Test.multiple
        (Staged.stage test_pop) )
  , if safe then
      Some
        (Test.make_with_resource ~name ~allocate:domains_allocate
           ~free:domains_free Test.uniq (Staged.stage domains_run) )
    else None )

let tests modules =
  let test_push_pop, test_many =
    modules
    |> List.map (fun (name, safe, m) -> test_fifo ~name ~safe m)
    |> List.split
  in
  let test_push, test_pop = List.split test_push_pop in
  Test.make_grouped ~name:"queue"
    [ Test.make_grouped ~name:"push" test_push
    ; Test.make_grouped ~name:"pop" test_pop
    ; Test.make_grouped ~name:"many" @@ List.filter_map Fun.id test_many ]

let () =
  Printf.printf "domain_count: %d" domain_count ;
  Bechamel_simple_cli.cli
    (tests
       [ (* ("unsafe", false, (module Fifo_unsafe))*)
         ("locked", true, (module Fifo_locked))
       ; ("lockfree", true, (module Fifo_lockfree)) ] )
