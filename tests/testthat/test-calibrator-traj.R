test_that("bad outputs give warnings", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  expect_warning(
    mp_simulator(sir, time_steps = 5, outputs = c("Infection")),
    regexp = "The following outputs were requested but not available in the model"
  )
  sir_sims = "TRAJ-sir_5_I.rds" |> test_cache_read()
  expect_warning(
    mp_tmb_calibrator(sir
      , data = sir_sims
      , traj = "I"
      , par = "beta"
      , outputs = c("E","I") # E doesn't exist
    ),
    regexp = "The following outputs were requested but not available in the model"
  )
  
  sir_sim = "TRAJ-sir_5_infection.rds" |> test_cache_read()

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
  sir_sims = "TRAJ-sir_5_state.rds" |> test_cache_read()
  
  sir_cal = mp_tmb_calibrator(sir
    , data = sir_sims
    # trajectories specified with likelihood distributions
    , traj = list(
        S = mp_neg_bin(disp = 0.5),
        I = mp_neg_bin(disp = 0.5),
        R = mp_log_normal(sd = 1)
    )
    , par = "beta"
    # outputs left to default
  ) 
  
  expect_identical(c("S","I","R")
   , sir_cal$cal_spec$must_save
  )
  
})

test_that("missing required columns in calibration data throw errors", {
  sir = "SPEC-sir.rds" |> test_cache_read()
  sir_sims = "TRAJ-sir_5_state.rds" |> test_cache_read()
  err = "Supplied data did not contain a column called"
  expect_error(mp_tmb_calibrator(sir, data = select(sir_sims, -time)), err)
  expect_error(mp_tmb_calibrator(sir, data = select(sir_sims, -matrix)), err)
  expect_error(mp_tmb_calibrator(sir, data = select(sir_sims, -value)), err)
})

test_that("vector-valued trajectories can be calibrated to", {
  skip("Skipping because rbind_time is not working on non-scalars")
  sir_age = "SPEC-sir_age.rds" |> test_cache_read()
  sir_age_sims = "TRAJ-sir_age_10_infection.rds" |> test_cache_read()
  sir_age_cal = mp_tmb_calibrator(sir_age
    , data = sir_age_sims
    , par = "tau"
    , traj = "infection"
    , outputs = "sim_infection"
  )
  sir_age_cal
  expect_equal(
      sir_age_cal$cal_spec |> mp_simulator(10, "infection") |> mp_trajectory() |> pull(value)
    , sir_age_cal$cal_spec |> mp_simulator(10, "sim_infection") |> mp_final() |> pull(value)
  )
})
