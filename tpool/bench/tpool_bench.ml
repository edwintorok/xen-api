open Bechamel
open Test_internal

let domain_count =
  if Sys.ocaml_release.major < 5 then Cpu.numcores ()
  else Domain.recommended_domain_count ()

let test_fifo ~name (module F : Types.FIFO) =
  let allocate () =
    let q = F.create () in
    for i = 1 to 1000 do
      F.push q i
    done ;
    q
  in
  let test_push t = F.push t 42 and test_pop t = F.pop_opt t in
  let push_count = 1 in
  let mutex = Mutex.create ()
  and condition = Condition.create ()
  and entered = (Atomic.make 0, Atomic.make 0)
  and finished = (Atomic.make 0, Atomic.make 0)
  and test_done = Atomic.make false
  and domains_q = F.create ()
  and domains_pushed = Atomic.make 0
  and domains_popped = Atomic.make 0
  and expected = domain_count * push_count in
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
        F.push domains_q i ; Atomic.incr domains_pushed
      done ;
      barrier finished ;
      barrier entered
    done
  and run_pop () =
    barrier entered ;
    while not (Atomic.get test_done) do
      while Atomic.get domains_popped < expected do
        (* wait for items to be pushed *)
        while F.pop_opt domains_q = None do
          Domain.cpu_relax ()
        done ;
        Atomic.incr domains_popped
      done ;
      barrier finished ;
      barrier entered
    done
  in
  let domains_alloc () =
    reset_barrier (fst entered);
    reset_barrier (snd entered);
    reset_barrier (fst finished);
    reset_barrier (snd finished);
    Atomic.set domains_popped 0;
    Atomic.set domains_pushed 0;
    Atomic.set test_done false;
    ( Array.init (domain_count / 2) (fun _ -> Domain.spawn run_push)
    , Array.init (domain_count / 2) (fun _ -> Domain.spawn run_pop) )
  in
  let domains_free (a, b) =
    Atomic.set test_done true ;
    barrier entered ;
    Array.iter Domain.join a ;
    Array.iter Domain.join b
  in
  let domains_run (_, _) = barrier entered ; barrier finished in
  ( ( Test.make_with_resource ~name ~allocate:F.create ~free:ignore
        Test.multiple (Staged.stage test_push)
    , Test.make_with_resource ~name ~allocate ~free:ignore Test.multiple
        (Staged.stage test_pop) )
  , Test.make_with_resource ~name ~allocate:domains_alloc ~free:domains_free
      Test.uniq (Staged.stage domains_run) )

let tests modules =
  let test_push_pop, test_many =
    modules |> List.map (fun (name, m) -> test_fifo ~name m) |> List.split
  in
  let test_push, test_pop = List.split test_push_pop in
  Test.make_grouped ~name:"queue"
    [ (* Test.make_grouped ~name:"push" test_push
         ; Test.make_grouped ~name:"pop" test_pop*)
      Test.make_grouped ~name:"many" test_many ]

let () =
  Bechamel_simple_cli.cli
    (tests
       [ ("unsafe", (module Fifo_unsafe))
       ; ("lockfree", (module Fifo_lockfree))
         (*; ("locked", (module Fifo_locked))*) ] )
