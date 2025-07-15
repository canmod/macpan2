library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2); library(deSolve)
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
        I = mp_lnorm(location = mp_fit(80), sd = mp_fit(1))
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
          I = mp_lnorm(location = mp_fit(80), sd = mp_fit(1)) 
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
     , par = list(beta = mp_lnorm(location = 1, sd = 1))
     , tv = "beta"
     , default = list(N = 300)
    )
    , regexp = "contains zeros at the beginning of the simulation"
  )
  
  data = "TRAJ-sir_50_infection.rds" |> test_cache_read()
  expect_error(
    mp_tmb_calibrator(
        sir_spec
      , data
      , traj = "infection"
      , par = list(beta = mp_norm(sd = mp_fit(0.1)))
      , default = list(beta = 0.25)
    )
    , regexp = "The following distributions do not have location parameters"
  )
})

test_that("you can specify uniform priors but not uniform likelihoods", {
  
  sir_spec = mp_tmb_library("starter_models"
    , "sir"
    , package = "macpan2"
  )
  sir_sim = "TRAJ-sir_50_I.rds" |> test_cache_read()
  
  # uniform likelihood
  expect_error(mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_unif())
    , par = c("beta")
    )
    , regexp = "You cannot specify uniform likelihoods"
  )
  
  # uniform prior
  specified_prior = mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_nbinom(disp = mp_fit(2)))
    , par = list(beta = mp_unif())
  )  
  default_prior = mp_tmb_calibrator(sir_spec
    , data = sir_sim
    , traj = list(I = mp_nbinom(disp = mp_fit(2)))
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
    , par = list(beta_values = mp_norm(location = c(0.3,0.2), sd = 1))
    , default = list(N = 300)
    ), regexp = "has more than one element"
  )
  
  expect_error(mp_tmb_calibrator(spec
    , data = sir_data
    , traj = list(I = mp_nbinom(disp = c(1,5)))
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
   , traj = list(I = mp_norm(sd = mp_fit("sd")))
   , par = "beta"
  )
  
  # don't fit sd, but use existing variable in the model
  nofit_char = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_norm(sd = mp_nofit("sd")))
   , par = "beta"
  )
  
  # fit sd with a starting value of 1.2
  fit_num = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_norm(sd = mp_fit(1.2)))
   , par = "beta"
  )
  
  # don't fit sd, use 1.2 as default
  nofit_num = mp_tmb_calibrator(spec
   , data = sir_data
   , traj = list(I = mp_norm(sd = mp_nofit(1.2)))
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

  spec_emptydefault = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    |> mp_tmb_insert(default = list(sd = empty_matrix))
  )
  spec_nodefault = (mp_tmb_library("starter_models", "sir", package = "macpan2")
    # insert dummy expression to add variable to the spec
    |> mp_tmb_insert(expressions = list(sd ~ sd))
  )
  sir_data = (mp_simulator(spec_emptydefault, time_steps = 5, outputs = c("I"))
    |> mp_trajectory()
  )
  
  # character misspecification in mp_fit, variable doesn't exist in model
  expect_error(mp_tmb_calibrator(spec_emptydefault
     , data = sir_data
     , traj = list(I = mp_norm(sd = mp_fit("Sd")))
     , par = "beta"
    )
    , regexp = "Sd is not in the model spec"
  )
  
  # correct specification in mp_fit, but variable default is `empty_matrix`
  expect_error(mp_tmb_calibrator(spec_emptydefault
     , data = sir_data
     , traj = list(I = mp_norm(sd = mp_fit("sd")))
     , par = "beta"
    )
  )
  
  # correct specification in mp_fit, but variable has no default
  expect_error(mp_tmb_calibrator(spec_nodefault
     , data = sir_data
     , traj = list(I = mp_norm(sd = mp_fit("sd")))
     , par = "beta"
    )
  , regexp = "sd is not in the model spec"
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
    , par = list(beta = mp_norm(mp_nofit(12,mp_log),mp_nofit(34,mp_identity)))
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
    , par = list(beta = mp_norm(12,"sd"))
  )
  
  mp_nofit_cal = mp_tmb_calibrator(spec
    , data = sir_data
    , traj = "I"
    , par = list(beta = mp_norm(mp_nofit(12),mp_nofit("sd")))
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

test_that("mp_nbinom replaces mp_neg_bin", {
  spec = test_cache_read("SPEC-sir.rds")
  data = test_cache_read("TRAJ-sir_50_infection.rds")
  expect_warning(neg_bin <- mp_neg_bin(disp = 1), regexp = "is deprecated")
  cal_neg_bin = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = list(infection = neg_bin)
    , par = "beta"
    , default = list(beta = 0.25)
  )
  cal_nbinom = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = list(infection = mp_nbinom(disp = 1))
    , par = "beta"
    , default = list(beta = 0.25)
  )
  opt_neg_bin = mp_optimize(cal_neg_bin)
  opt_nbinom = mp_optimize(cal_nbinom)
  expect_identical(opt_neg_bin, opt_neg_bin)
})


test_that("mp_pois replaces mp_poisson", {
  spec = test_cache_read("SPEC-sir.rds")
  data = test_cache_read("TRAJ-sir_50_infection.rds")
  expect_warning(poisson <- mp_poisson(), regexp = "is deprecated")
  cal_poisson = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = list(infection = poisson)
    , par = "beta"
    , default = list(beta = 0.25)
  )
  cal_pois = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = list(infection = mp_pois())
    , par = "beta"
    , default = list(beta = 0.25)
  )
  opt_poisson = mp_optimize(cal_poisson)
  opt_pois = mp_optimize(cal_pois)
  expect_identical(opt_poisson, opt_pois)
})


test_that("mp_norm replaces mp_normal", {
  spec = test_cache_read("SPEC-sir.rds")
  data = test_cache_read("TRAJ-sir_50_infection.rds")
  expect_warning(normal <- mp_normal(0.25, 0.1), regexp = "is deprecated")
  cal_normal = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = "infection"
    , par = list(beta = normal)
    , default = list(beta = 0.25)
  )
  cal_norm = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = "infection"
    , par = list(beta = mp_norm(0.25, 0.1))
    , default = list(beta = 0.25)
  )
  opt_normal = mp_optimize(cal_normal)
  opt_norm = mp_optimize(cal_norm)
  expect_identical(opt_normal, opt_norm)
})

test_that("mp_unif replaces mp_uniform", {
  spec = test_cache_read("SPEC-sir.rds")
  data = test_cache_read("TRAJ-sir_50_infection.rds")
  expect_warning(uniform <- mp_uniform(), regexp = "is deprecated")
  cal_uniform = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = "infection"
    , par = list(beta = uniform)
    , default = list(beta = 0.25)
  )
  cal_unif = mp_tmb_calibrator(
      spec = spec
    , data = data
    , traj = "infection"
    , par = list(beta = mp_unif())
    , default = list(beta = 0.25)
  )
  opt_uniform = mp_optimize(cal_uniform)
  opt_unif = mp_optimize(cal_unif)
  expect_identical(opt_uniform, opt_unif)
})
