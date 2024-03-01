test_that("bad outputs give warnings", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sir_sim = mp_simulator(sir, time_steps = 5, outputs = c("infection")) |> mp_trajectory()

  expect_error(
    mp_tmb_calibrator(sir
      , data = sir_sim
      , traj = "infection"
      , par = "Beta"
    )
    , regexp = "Requested parameters \\(including Beta\\) are not available in the model spec"
  )

})
