test_that("dynamic variables interact properly with the state updater", {
  spec = test_cache_read("SPEC-macpan_base.rds")
  euler_dyn_vars = spec |> mp_dynamic_vars()
  euler_state_flow_vars = spec |> mp_state_flow_vars()
  expect_in(euler_dyn_vars
    , spec |> mp_rk4() |> mp_dynamic_vars()
  )
  expect_setequal(euler_dyn_vars
    , spec |> mp_hazard() |> mp_dynamic_vars()
  )
  expect_setequal(euler_dyn_vars
    , spec |> mp_discrete_stoch() |> mp_dynamic_vars()
  )
  expect_setequal(euler_state_flow_vars
    , spec |> mp_rk4() |> mp_state_flow_vars()
  )
  expect_setequal(euler_state_flow_vars
    , spec |> mp_hazard() |> mp_state_flow_vars()
  )
  expect_setequal(euler_state_flow_vars
    , spec |> mp_discrete_stoch() |> mp_state_flow_vars()
  )
  
})
