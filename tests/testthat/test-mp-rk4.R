library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("mp_rk4() does not repeat preceding random computations to flows", {
  
  seir = mp_tmb_library("starter_models","seir",package = "macpan2")
  seir_gamma = mp_tmb_insert(seir
   , phase = "during"
   , at = 1L
   , expressions = list(gamma ~ rnorm(0.1,0.05))
  )
  
  set.seed(1L)
  euler_seir = seir_gamma |> mp_simulator(10, "gamma") |> mp_trajectory()
  set.seed(1L)
  rk4_seir = seir_gamma |> mp_rk4() |> mp_simulator(10,"gamma") |> mp_trajectory()
  expect_equal(euler_seir, rk4_seir)
})

test_that("mp_rk4() does not interfere with preceding time step dependent functions", {

  change_pts = c(0,5,6)
  beta_diffs = c(0.2,0.8,-0.5)
  beta_base = 1.1
  seir = mp_tmb_library("starter_models","seir",package = "macpan2")
  seir_beta = mp_tmb_insert(seir
                            , phase="during"
                            , at = 1L
                            , expressions = list(beta ~ beta + time_var(beta_diffs,change_pts))
                            , default = list(beta = beta_base, beta_diffs = beta_diffs)
                            , integers = list(change_pts = change_pts))
  
  euler_seir = seir_beta |> mp_euler() |> mp_simulator(10,c("beta")) |> mp_trajectory()
  rk4_seir = seir_beta |> mp_rk4() |> mp_simulator(10,c("beta")) |> mp_trajectory()
  
  expect_equal(euler_seir, rk4_seir)
})

test_that("mp_rk4() does not repeat formulas that assign values to state variables", {

  si = mp_tmb_library("starter_models", "si", package = "macpan2")
  si_dumb = mp_tmb_insert(si
     , phase = "during"
     , at = 1L
     , expressions = list(S ~ 1)
  )
  
  ## exactly one change in the during phase of the rk4 version of the model
  ## should be a formula (in particular S ~ 1) and all others should be
  ## PerCapitaFlow objects.
  n_formulas = mp_rk4(si_dumb)$during |> vapply(macpan2:::is_two_sided, logical(1L)) |> sum()
  expect_equal(n_formulas, 1L)
})

test_that("mp_rk4() gives the same state variable updates as mp_rk4_old()", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")

  new = sir |> mp_rk4()     |> mp_simulator(30L, c("I", "infection")) |> mp_trajectory()
  old = sir |> mp_rk4_old() |> mp_simulator(30L, c("I", "infection")) |> mp_trajectory()
  
  I     = filter(new, matrix == "I")
  I_old = filter(old, matrix == "I")
  infection     = filter(new, matrix == "infection")
  infection_old = filter(old, matrix == "infection")
  
  expect_equal(I, I_old)
  expect_gt(mean(infection_old$value), mean(infection$value))
})
