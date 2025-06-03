test_that("absolute inflow definition and ode trajectory", {
    ## exponential decay
    s1 = test_cache_read("SPEC-one_box.rds")
    
    expect_equal(mp_state_vars(s1), "N")
    expect_equal(mp_flow_vars(s1), c("importation", "death"))
    
    steps = 50
    
    solution = s1 |> mp_rk4() |> mp_simulator(steps, "N") |> mp_trajectory()
    
    state = s1$default[c("N")]
    params = s1$default[c("r", "d")]
    reference_solution <- ode(
        y = unlist(state)
      , times = 0:steps
      , func = \(t, state, params) with(c(params, state), list(r - d * N))
      , parms = params
      , method = "rk4"
    )
    
    expect_equal(reference_solution[-1, "N"], solution[, "value"])
})


test_that("stochastic absolute importation is plausible", {
  s2 = mp_tmb_model_spec(
      during = list(
          N ~ S + I
        , mp_per_capita_flow("S", "I", "beta * I / N", "infection")
        , mp_inflow("I", "mu", "importation")
      )
    , default = list(S = 100, I = 0, beta = 0.25, mu = 0.02)
  )
  set.seed(7)
  traj = (s2 
    |> mp_discrete_stoch() 
    |> mp_simulator(10, "I") 
    |> mp_trajectory_par()
  )
  answer = structure(list(matrix = c("I", "I", "I", "I", "I", "I", "I", 
  "I", "I", "I"), time = 1:10, row = c(0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0), col = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), value = c(1, 1, 
  1, 1, 1, 1, 1, 1, 1, 3)), row.names = c(NA, -10L), class = "data.frame")
  expect_equal(traj, answer)
})
