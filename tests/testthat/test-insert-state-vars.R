test_that("inserted expressions are able to refer to single states/flows", {
  spec = mp_tmb_model_spec(
      before = list(N ~ sum(state))
    , during = list(
        per_capita[foi] ~ beta * state[I] / N
      , flow_rates ~ per_capita * state[from]
      , inflow ~ group_sums(flow_rates, to, state)
      , outflow ~ group_sums(flow_rates, from, state)
      , state ~ state + inflow - outflow
    )
    , default = list(
        state = c(S = 99, I = 1, R = 0)
      , per_capita = c(foi = NA, gamma = 0.2)
      , beta = 0.4
    )
    , integers = list(
        from = c(0, 1)
      , to = c(1, 2)
    )
  )
  s = (spec
    |> mp_tmb_insert(at = Inf
      , expressions = list(test_ratio ~ state[I] / state[S])
    )
    |> mp_simulator(time_steps = 50
      , outputs = c("S", "I", "test_ratio")
    )
  )
  v = mp_trajectory(s)
  expect_equal(
    v[v$row == "I", "value"] / v[v$row == "S", "value"],
    v[v$matrix == "test_ratio", "value"]
  )
})
