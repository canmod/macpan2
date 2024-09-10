library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("bad outputs give warnings", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  expect_warning(
    mp_simulator(sir, time_steps = 5, outputs = c("Infection")),
    regexp = "The following outputs were requested but not available in the model"
  )
  sir_sims = mp_simulator(sir, time_steps = 5, outputs = c("I")) |> mp_trajectory()
  expect_warning(
    mp_tmb_calibrator(sir
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
      , traj = "Infection"
      , par = "beta"
    )
    , regexp = "are not available in the data"
  )

  # traj doesn't exist in model spec
  expect_error(
    mp_tmb_calibrator(sir
      , data = mutate(sir_sim, matrix = "infectious")
      , traj = "infectious"
      , par = "beta"
    )
    , regexp = "are not available in the model spec"
  )

})
test_that("trajectories specified with likelihood distributions end up in calibrator outputs", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sir_sims = mp_simulator(sir, time_steps = 5, outputs = c("I","R")) |> mp_trajectory()
  
  sir_cal = mp_tmb_calibrator(sir
    , data = sir_sims
    # trajectories specified with likelihood distributions
    , traj = list(
        I = mp_neg_bin(disp = 0.5),
        R = mp_log_normal(sd = 1)
    )
    , par = "beta"
    # outputs left to default
  ) 
  
  expect_identical(c("I","R")
   , sir_cal$cal_spec$must_save
  )
  
})
