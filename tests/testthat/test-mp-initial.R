library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("initial values in model specifications and model simulators are consistent up to row order", {

  sir_spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sir_sim = mp_simulator(sir_spec, 5, "I")
  
  initial_spec = mp_initial(sir_spec)
  initial_sim = mp_initial(sir_sim)
  initial_list_spec = mp_initial_list(sir_spec)
  initial_list_sim = mp_initial_list(sir_sim)
  
  expect_equal(
      initial_spec[order(initial_spec$matrix), , drop = FALSE]
    , initial_sim[order(initial_sim$matrix), , drop = FALSE]
    , ignore_attr = TRUE
  )
  expect_equal(initial_list_spec, initial_list_sim)
})
