open Bechamel
open Test_internal

let domain_count =
  ( if Sys.ocaml_release.major < 5 then Cpu.numcores ()
    else Domain.recommended_domain_count () )
  - 1

module Domains = struct
  let rec worker complete data work stop i () =
    if Atomic.get stop then ()
    else
      match Atomic.exchange work None with
      | None ->
          Thread.yield ();
          Domain.cpu_relax () ;
          (worker [@tailcall]) complete data work stop i ()
      | Some f ->
          let () =
            try f stop data i
            with e ->
              Atomic.set stop true ;
              let bt = Printexc.get_raw_backtrace () in
              Printexc.print_raw_backtrace stderr bt ;
              prerr_endline (Printexc.to_string e)
          in
          Atomic.incr complete ;
          (worker [@tailcall]) complete data work stop i ()

  let alloc create () =
    let data = create ()
    and stop = Atomic.make false
    and complete = Atomic.make 0 in
    ( data
    , complete
    , stop
    , Array.init domain_count (fun i ->
          let work = Atomic.make None in
          (work, Domain.spawn @@ worker complete data work stop i) ) )

  let run_on (_, complete, _, t) i f =
    let work, _ = t.(i) in
    if not (Atomic.compare_and_set work None (Some f)) then
      invalid_arg "Domain already running a task" ;
    Atomic.decr complete

  let join (_, d) = Domain.join d

  let wait_all (_, count, stop, _) =
    while (not (Atomic.get stop)) && Atomic.get count < 0 do
          Thread.yield ();
      Domain.cpu_relax ()
    done

  let free (_, _, stop, t) =
    Atomic.set stop true ;
    Array.iter join t ;
    Gc.full_major () ;
    Gc.full_major () ;
    Gc.compact ()

  let data (d, _, _, _) = d
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
          Thread.yield ();
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
    let q, domains_pushed, domains_popped = Domains.data t in
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
