if (interactive()) {
  library(macpan2); library(testthat); library(deSolve)
  source("tests/testthat/setup.R")
}
test_that("rk4 time-steps work with the si model", {
  delta_t = 0.1
  time = 50
  si = test_cache_read("SPEC-si.rds") |> mp_rk4(delta_t)
  solution = si |> mp_simulator(time/delta_t, "I") |> mp_trajectory()
  state = si$default["I"]
  params = si$default[c("beta", "N")]
  reference_solution <- ode(
      y = unlist(state)
    , times = seq(from = 0, to = time, by = delta_t)
    , func = \(t, state, params) with(c(params, state), list((N - I) * (beta * I/N)))
    , parms = params
    , method = "rk4"
  )
  expect_equal(solution$value, reference_solution[-1, "I"])
})

test_that("rk4 time-steps work with absolute inflows", {
  delta_t = 0.1
  time = 50
  one_box = test_cache_read("SPEC-one_box.rds") |> mp_rk4(delta_t)
  solution = one_box |> mp_simulator(time/delta_t, "N") |> mp_trajectory()
  state = one_box$default["N"]
  params = one_box$default[c("r", "d")]
  reference_solution <- ode(
      y = unlist(state)
    , times = seq(from = 0, to = time, by = delta_t)
    , func = \(t, state, params) with(c(params, state), list(r - d * N))
    , parms = params
    , method = "rk4"
  )
  expect_equal(solution$value, reference_solution[-1, "N"])
})

test_that("stochastic absolute importation with a time step is plausible", {
  spec = mp_tmb_model_spec(
      during = list(
          N ~ S + I
        , mp_per_capita_flow("S", "I", "beta * I / N", "infection")
        , mp_inflow("I", "mu", "importation")
      )
    , default = list(S = 100, I = 0, beta = 0.8, mu = 0.1)
  )
  time = 10
  delta_t = 0.1
  set.seed(1L)
  traj = (spec
    |> mp_discrete_stoch(delta_t)
    |> mp_simulator(time / delta_t, "I") 
    |> mp_trajectory_sim(10, probs = 0.5)
    |> mutate(time = delta_t * time)
    |> filter(time == round(time))
  )
  row.names(traj) = 1:10 ## seems this is needed for testthat -- not sure why
  answer = structure(list(matrix = c("I", "I", "I", "I", "I", "I", "I", 
  "I", "I", "I"), time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), row = c(0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0), col = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0), `value_50%` = c(0, 0, 0, 0, 0, 0, 0, 0, 2, 3)), class = "data.frame", 
  row.names = 1:10)
  
  expect_equal(traj, answer)
})
