module Make (S : Lin.Spec) = struct
  module L = Lin_thread.Make (S)

  (* without a lot of [Thread.yield] inserted strategically in the data structure implementation itself,
     [Lin_thread] can't really detect bugs on OCaml 4.
     So just skip the tests
  *)

  let lin_test ~count:_ ~name =
    (* we run 1 test to check the code compiles and works at all on OCaml 4 *)
    L.lin_test ~count:1 ~name

  let neg_lin_test ~count:_ ~name =
    (* skip test, but 0 tests will always succeed, so invert polarity *)
    L.lin_test ~count:0 ~name
end
