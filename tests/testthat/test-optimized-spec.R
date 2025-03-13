test_that("optimized specs can be generated", {
  options(macpan2_verbose = FALSE)
  spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim = mp_simulator(spec, 50, "infection")
  data = mp_trajectory(sim)
  cal = mp_tmb_calibrator(
      spec
    , data
    , traj = "infection"
    , par = "beta"
    , default = list(beta = 0.25)
  )
  mp_optimize(cal)
  expect_identical(
      mp_optimized_spec(cal)$default$beta
    , mp_optimizer_output(cal)$par |> unname()
  )
  expect_equal( ## not identical due to numerical optimization
      spec$default$beta
    , mp_optimizer_output(cal)$par |> unname()
  )
})
