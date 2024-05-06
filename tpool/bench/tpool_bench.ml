open Bechamel
open Test_internal

let domain_count =
  if Sys.ocaml_release.major < 5 then Cpu.numcores ()
  else Domain.recommended_domain_count ()


module DomainsTest = struct
  let push_count = 100

  let expected = (domain_count / 2) * push_count

  let alloc create f1 f2 =
    let mutex = Mutex.create ()
    and condition = Condition.create ()
    and entered = (Atomic.make 0, Atomic.make 0)
    and finished = (Atomic.make 0, Atomic.make 0)
    and test_done = Atomic.make false
    and data = create ()
    and domains_pushed = Atomic.make 0
    and domains_popped = Atomic.make 0 in
    let wait_barrier b =
      Mutex.lock mutex ;
      Atomic.incr b ;
      while Atomic.get b <= domain_count do
        Condition.wait condition mutex
      done ;
      Mutex.unlock mutex ;
      Condition.broadcast condition
    in
    let reset_barrier b = Atomic.set b 0 in
    let barrier (b1, b2) =
      reset_barrier b2 ; wait_barrier b1 ; wait_barrier b2 ; reset_barrier b1
    in
    let run_push () =
      barrier entered ;
      while not (Atomic.get test_done) do
        for i = 1 to push_count do
          f1 data i domains_pushed domains_popped
        done ;
        barrier finished ;
        barrier entered
      done
    and run_pop () =
      barrier entered ;
      while not (Atomic.get test_done) do
        while Atomic.get domains_popped < expected do
          f2 data domains_pushed domains_popped
        done ;
        barrier finished ;
        barrier entered
      done
    in
    let domains1 =
      Array.init (domain_count / 2) (fun _ -> Domain.spawn run_push)
    and domains2 =
      Array.init (domain_count / 2) (fun _ -> Domain.spawn run_pop)
    in
    let free () =
      Atomic.set test_done true ;
      barrier entered ;
      Array.iter Domain.join domains1 ;
      Array.iter Domain.join domains2
    in
    let run () = barrier entered ; barrier finished in
    (run, free)

  let free (_, f) = f ()
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
  let run_push t x domains_pushed _ = F.push t x ; Atomic.incr domains_pushed in
  let rec run_pop t domains_pushed domains_popped =
    if Atomic.get domains_popped < DomainsTest.expected then
      if F.pop_opt t = None then (
        Domain.cpu_relax () ;
        if
          Atomic.get domains_pushed = DomainsTest.expected
          && Atomic.get domains_popped < DomainsTest.expected
        then
          if F.pop_opt t = None then
            invalid_arg
              (Printf.sprintf "All %d pushed, but only got %d"
                 DomainsTest.expected
                 (Atomic.get domains_popped) )
          else Atomic.incr domains_popped
        else (run_pop [@tailcall]) t domains_pushed domains_popped )
      else Atomic.incr domains_popped
  in
  let domains_allocate () = DomainsTest.alloc F.create run_push run_pop
  and domains_free = DomainsTest.free
  and domains_run (run, _) = run () in
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
  Bechamel_simple_cli.cli
    (tests
       [ ("unsafe", false, (module Fifo_unsafe))
       ; ("lockfree", true, (module Fifo_lockfree))
       ; ("locked", true, (module Fifo_locked)) ] )
