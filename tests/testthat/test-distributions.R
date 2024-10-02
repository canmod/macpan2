library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("distributions give appropriate variable assumption warnings", {
  
  # At this time the only distribution with variable assumptions is the 
  # log-normal.
  sir_spec = mp_tmb_library("starter_models"
    , "sir"
    , package = "macpan2"
  )
  # Create observed prevalence data.
  sir_prevalence = data.frame(
      matrix = "I"
    , time = seq(1:10)
    , row = 0
    , col = 0
    , value = rpois(10, 20) + 1 # add 1 to ensure I > 0
  )
  # Variable assumption is not violated here.
  expect_no_warning(mp_tmb_calibrator(sir_spec
    , data = sir_prevalence
    , traj = list(
        I = mp_log_normal(location = mp_fit(80), sd = mp_fit(1))
      )
    , par = c("beta")
    , default = list(N = 300)
    )
  )
  
  # Set one observed prevalence value to zero
  sir_prevalence$value[5] = 0
  # The variable assumption is violated here. For the log-normal distribution,
  # the variable cannot be zero.
  expect_warning(mp_tmb_calibrator(sir_spec
      , data = sir_prevalence
      , traj = list(
          I = mp_log_normal(location = mp_fit(80), sd = mp_fit(1)) 
        )
      , par = c("beta")
      , default = list(N = 300)
    )
    , regexp = "contains zeros at the beginning of the simulation"
  )
  
  # Create time-varying beta values
  sir_beta = data.frame(
      matrix = "beta"
    , time = c(1,5)
    , value = c(0.25, 0)
  )
  # The variable assumption is violated here. For the log-normal distribution,
  # the variable cannot be zero.
  expect_warning(mp_tmb_calibrator(sir_spec
   , data = bind_rows(sir_prevalence, sir_beta)
   , traj = "I"
   , par = list(beta = mp_log_normal(location = 1, sd = 1))
   , tv = "beta"
   , default = list(N = 300)
  )
  , regexp = "contains zeros at the beginning of the simulation"
  )
  
})

test_that("you can specify uniform priors but not uniform likelihoods", {
  
  sir_spec = mp_tmb_library("starter_models"
    , "sir"
    , package = "macpan2"
  )
  sir_sim = mp_simulator(sir_spec, 10, "I") |> mp_trajectory()
  
  # uniform likelihood
  expect_error(mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_uniform())
    , par = c("beta")
    )
    , regexp = "You cannot specify uniform likelihoods"
  )
  
  # uniform prior
  specified_prior = mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_neg_bin(disp = mp_fit(2)))
    , par = list(beta = mp_uniform())
  )  
  default_prior = mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_neg_bin(disp = mp_fit(2)))
    , par = c("beta")
   )
  expect_equal(specified_prior$cal_spec$all_matrices(), default_prior$cal_spec$all_matrices())
  
})

test_that("distributional parameters cannot be vectors (for now)", {
  spec = (mp_tmb_library("starter_models", "sir", package = "macpan2")
          |> mp_tmb_insert(default = list(beta_values = c(0.3, 0.1, 0.4)))
  )
  sir_data = (
    mp_simulator(spec, time_steps = 5, outputs = c( "I", "beta"))
    |> mp_trajectory()
  )
  
  expect_error(mp_tmb_calibrator(spec
    , data = sir_data
    , traj = "I"
    , par = list(beta_values = mp_normal(location = c(0.3,0.2), sd = 1))
    , default = list(N = 300)
    ), regexp = "has more than one element"
  )
  
  expect_error(mp_tmb_calibrator(spec
    , data = sir_data
    , traj = list(I = mp_neg_bin(disp = c(1,5)))
    , par = "beta"
    , default = list(N = 300)
    ), regexp = "has more than one element"
  )
})