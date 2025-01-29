library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("optimizer-specific arguments are passed (#240)", {
  options(macpan2_verbose = FALSE)
  
  ## setup (from calibration vignette)
  sir_spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sir_simulator = mp_simulator(sir_spec
    , time_steps = 100
    , outputs = c("S", "I", "R")
    , default = list(N = 300, R = 100, beta = 0.25, gamma = 0.1)
  )
  set.seed(101)
  sir_results = mp_trajectory(sir_simulator)
  sir_prevalence = (sir_results
      |> dplyr::select(-c(row, col))
      |> filter(matrix == "I")
      |> rename(true_value = value)
      |> mutate(value = rnbinom(n(), mu = true_value, size = 2))
  )
  
  do_opt = function(method = "official") {
    sir_calibrator = mp_tmb_calibrator(sir_spec
       , data = sir_prevalence
       , traj = "I"
       , par = c("beta", "R")
       , default = list(N = 300)
    )
    switch(method
      , official = mp_optimize(sir_calibrator, control = list(iter.max = 1000))
      , by_hand = with(mp_tmb(sir_calibrator), nlminb(par, fn, gr, he, control = list(iter.max = 1000)))
      , internal = sir_calibrator$simulator$optimize[["nlminb"]](control = list(iter.max = 1000))
    )
  }
  official = do_opt("official")
  by_hand = do_opt("by_hand")
  internal = do_opt("internal")
  expect_equal(official, by_hand)
  expect_equal(official, internal)

})
