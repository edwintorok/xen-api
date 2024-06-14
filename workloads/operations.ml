module Yield = struct
  let count = Atomic.make 0

  let perform () =
    Thread.yield ();
    Atomic.incr count

  let worker = perform
end

