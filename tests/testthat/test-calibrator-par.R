test_that("bad parameterizations give errors", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sir_sim = mp_simulator(sir, time_steps = 5, outputs = c("infection")) |> mp_trajectory()

  expect_error(
    mp_tmb_calibrator(sir
      , data = sir_sim
      , traj = "infection"
      , par = "Beta"
    )
  )

  expect_error(
    mp_tmb_calibrator(sir
      , data = sir_sim
      , traj = "infection"
      , par = "recovery"
    )
    #, regexp = "Requested parameters \\(including recovery\\) are not available in the model spec"
  )
})



