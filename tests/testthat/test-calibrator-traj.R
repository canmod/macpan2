test_that("bad outputs give warnings", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  expect_warning(
    mp_simulator(sir, time_steps = 5, outputs = c("Infection")),
    regexp = "The following outputs were requested but not available in the model"
  )
  sir_sims = mp_simulator(sir, time_steps = 5, outputs = c("I")) |> mp_trajectory()
  expect_warning(
    sir_calib <- mp_tmb_calibrator(sir
      , data = sir_sims
      , traj = "I"
      , par = "beta"
      , outputs = c("E","I") # E doesn't exist
    ),
    regexp = "The following outputs were requested but not available in the model"
  )
  
  sir_sim = mp_simulator(sir, time_steps = 5, outputs = c("infection")) |> mp_trajectory()

  # doesn't match sir_sim trajectory name
  expect_error(
    mp_tmb_calibrator(sir
      , data = sir_sim
      , traj = "beta"
      , par = "beta"
    )
    , regexp = "are not available in the data"
  )

  # traj doesn't exist in model spec
  expect_error(
    mp_tmb_calibrator(sir
      , data = sir_sim
      , traj = "Infection"
      , par = "beta"
    )
    , regexp = "are not available in the model spec"
  )

})
