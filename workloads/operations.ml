module Yield = struct
  let count = Atomic.make 0

  let perform () =
    Thread.yield ();
    Printf.eprintf "GET: %d\n%!" (Atomic.get count);
    Atomic.incr count

  let worker = perform
end

