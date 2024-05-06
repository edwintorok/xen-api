module Make (Spec : STM.Spec) = struct
  include STM_domain.Make (Spec)

  let agree_test = agree_test_par_asym

  let neg_agree_test = neg_agree_test_par_asym
end
