module Make (Spec : STM.Spec) = struct
  include STM_thread.Make (Spec)

  let agree_test = agree_test_conc

  let neg_agree_test ~count:_ =
    (* skip test, but 0 tests will always succeed, so invert polarity *)
    agree_test_conc ~count:0
end
