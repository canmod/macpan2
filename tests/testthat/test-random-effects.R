test_that("laplace approximation does something", {
  options(macpan2_verbose = FALSE)
  spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim = mp_simulator(spec, 50, "infection")
  data = mp_trajectory(sim)
  cal = mp_tmb_calibrator(
      spec
    , data
    , traj = "infection"
    , par = mp_par(
          params = list(beta = mp_normal(0.25, 1))
        , random = list(gamma = mp_normal(0.25, 1))
      )
    , default = list(beta = 0.25, gamma = 0.17)
  )
  suppressWarnings(mp_optimize(cal))
  expected_coef = structure(list(term = "params", mat = "beta", row = 0L, col = 0L, 
      default = 0.25, type = "fixed", estimate = 0.201234407482024, 
      std.error = 0.0359410524078107), row.names = c(NA, -1L), class = "data.frame")
  
  expect_equal(mp_tmb_coef(cal), expected_coef)
})
