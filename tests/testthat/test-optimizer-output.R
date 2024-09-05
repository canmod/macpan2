library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("all optimizer output is saved", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim_I = mp_simulator(sir,10,"I") |> mp_trajectory()
  
  sir_cal = mp_tmb_calibrator(sir
    , data = sim_I
    , traj = "I"
    , par = "beta"
  )
  # optimize twice
  mp_optimize(sir_cal)
  mp_optimize(sir_cal)
  expect_identical(length(mp_optimizer_output(sir_cal,"all")),2L)
})
