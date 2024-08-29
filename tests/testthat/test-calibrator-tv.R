library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("rbf basis dimension is the same size as initial weights", {
  spec = (
    mp_tmb_library("starter_models", "sir", package = "macpan2") 
      |> mp_tmb_insert("during", 1L
         , expressions = list(beta ~ beta0 * beta1)
         , default = list(beta0 = 0.5, beta1 = 1)
    )
  )
  sim = mp_simulator(spec, 10, c("R","beta")) |> mp_trajectory()

  expect_error(
    mp_tmb_calibrator(spec
      , data = sim
      , traj = "R"
      , par = "beta0"
      , tv = mp_rbf("beta1",3, c())
      , outputs = c("beta")
    ),
    regexp = "The `initial_weights` vector must be of length `dimension`"
  )
})

test_that("time-varying parameters values on input are consistent with calibrator output values", {
  spec = (
    mp_tmb_library("starter_models", "sir", package = "macpan2")
      |> mp_tmb_insert("during", 1L
        , expressions = list(beta ~ beta0 * beta1)
        , default = list(beta0 = 0.5, beta1 = 1)
    )
  )
  sim = mp_simulator(spec, 10, c("R","beta")) |> mp_trajectory()

  set.seed(10)
  beta1_val = runif(10)
  beta1_df = data.frame(matrix = "beta1", time = seq(1:10), value = beta1_val)

  expect_identical(
    mp_tmb_calibrator(spec
      , data = bind_rows(sim, beta1_df)
      , traj = "R"
      , par = "beta0"
      , tv = "beta1"
      , outputs = c("beta1")
    ) |> mp_trajectory() |> select(value) |> unlist(use.names = FALSE),
    beta1_val
  )
})

test_that("the default value can be changed for the prior standard deviation of rbf coefficients", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim = mp_simulator(sir, 10, "I") |> mp_trajectory()
  prior_sd_default = 2
  
  cal = mp_tmb_calibrator(sir
    , data = sim
    , traj = "I"
    , par = "beta"
    , tv = mp_rbf("gamma", 3, prior_sd = prior_sd_default)
  )
  
  mp_optimize(cal)
  coefs = mp_tmb_coef(cal)
  expect_identical(
      coefs |> filter(mat == "prior_sd_gamma") |> select(default) |> unlist(use.names = FALSE)
    , prior_sd_default
  )
})