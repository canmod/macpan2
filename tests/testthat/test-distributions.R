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

test_that("default distributional parameter transformation is consistent", {
  spec = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    # add 'sd' parameter to model spec and give it a default
    |> mp_tmb_insert(default = list(sd = 1.3))
  )
  sir_data = (mp_simulator(spec, time_steps = 5, outputs = c("I"))
    |> mp_trajectory()
  )
  
  # The following calibration cases are specified with a normal distribution
  # likelihood which has a default log transformation for the sd parameter.
  
  # fit sd distributional parameter as existing variable in the model
  fit_char = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_normal(sd = mp_fit("sd")))
   , par = "beta"
  )
  
  # don't fit sd, but use existing variable in the model
  nofit_char = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_normal(sd = mp_nofit("sd")))
   , par = "beta"
  )
  
  # fit sd with a starting value of 1.2
  fit_num = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_normal(sd = mp_fit(1.2)))
   , par = "beta"
  )
  
  # don't fit sd, use 1.2 as default
  nofit_num = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_normal(sd = mp_nofit(1.2)))
   , par = "beta"
  )
  
  # check that sd parameter is transformed regardless of fit/no_fit,
  # char/num specification
  expect_true(
    grepl("exp\\(sd\\)"
      , as.character(fit_char$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
      , perl = TRUE
    )
  )
  expect_true(
    grepl("exp\\(sd\\)"
      , as.character(nofit_char$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
      , perl = TRUE
    )
  )
  expect_true(
    grepl("log_sd"
      , as.character(fit_num$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
      , perl = TRUE
    )
  )
  expect_true(
    grepl("exp\\(1\\.2\\)"
      , as.character(nofit_num$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
      , perl = TRUE
    )
  )
  
})

test_that("misspecification of distributional parameters results in the appropriate errors",{

  spec = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    |> mp_tmb_insert(default = list(sd = empty_matrix))
  )
  sir_data = (mp_simulator(spec, time_steps = 5, outputs = c("I"))
    |> mp_trajectory()
  )
  
  # character misspecification in mp_fit, variable doesn't exist in model
  expect_error(mp_tmb_calibrator(spec
     , data = sir_data
     , traj = list(I = mp_normal(sd = mp_fit("Sd")))
     , par = "beta"
    )
    , regexp = "Sd is not in the model spec"
  )
})

test_that("default transformations for distributional parameters can be updated",{
  
  spec = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    |> mp_tmb_insert(default = list(sd = empty_matrix))
  )
  sir_data = (mp_simulator(spec, time_steps = 5, outputs = c("I"))
    |> mp_trajectory()
  )
  

  trans_update = mp_tmb_calibrator(spec
    , data = sir_data
    , traj = "I"
    , par = list(beta = mp_normal(mp_nofit(12,mp_log),mp_nofit(34,mp_identity)))
   )

  expect_true(
    grepl("exp\\(12\\)\\, 34"
          , as.character(trans_update$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
          , perl = TRUE
    )
  )
})

test_that("not fitting a distributional parameter is the same as mp_nofit",{
  
  spec = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    |> mp_tmb_insert(default = list(sd = 34))
  )
  sir_data = (mp_simulator(spec, time_steps = 5, outputs = c("I"))
    |> mp_trajectory()
  )
  nofit_cal = mp_tmb_calibrator(spec
    , data = sir_data
    , traj = "I"
    , par = list(beta = mp_normal(12,"sd"))
  )
  
  mp_nofit_cal = mp_tmb_calibrator(spec
    , data = sir_data
    , traj = "I"
    , par = list(beta = mp_normal(mp_nofit(12),mp_nofit("sd")))
  )
  
  # verify the objective function is identical
  expect_identical(
      as.character(nofit_cal$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
    , as.character(mp_nofit_cal$simulator$tmb_model$obj_fn$obj_fn_expr)[[2]]
  )
  
  mp_optimize(nofit_cal)
  mp_optimize(mp_nofit_cal)
  
  # verify the optimization is identical
  expect_identical(
      mp_tmb_coef(nofit_cal)
    , mp_tmb_coef(mp_nofit_cal)
  )
})
