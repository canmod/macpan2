test_that("absolute inflow definition and ode trajectory", {
    ## exponential decay
    s1 <- mp_tmb_model_spec(
          during = list(
                mp_inflow("N", "r", "birth")
              , mp_per_capita_outflow("N", "d", "death")
          )
        , default = list(N = 0, r = 0.3, d = 0.2)
    )
    
    expect_equal(mp_state_vars(s1), "N")
    expect_equal(mp_flow_vars(s1), c("birth", "death"))
    
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
    
    all.equal(reference_solution[-1, "N"], solution[, "value"])
})
