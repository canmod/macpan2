test_that("repeated flow names get flagged", {
  spec = mp_tmb_model_spec(
      during = list(
          mp_per_capita_flow("S", "I", "beta * I", "repeated_flow_name")
        , mp_per_capita_flow("I", "R", "gamma"   , "repeated_flow_name")
      )
    , default = list(S = 1 - 1e-5, I = 1e-5, beta = 0.25, gamma = 0.1)
  )
  spec$change_model$flow_frame()
  spec$change_model$duplicated_change_names()
  expect_error(
      spec$change_model$check()
    , "The following names are duplicates"
  )
  expect_error(
      mp_rk4(spec)$change_model$check()
    , "The following names are duplicates"
  )
})
