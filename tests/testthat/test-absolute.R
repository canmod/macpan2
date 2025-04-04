test_that("absolute inflow definition", {
    s1 <- mp_tmb_model_spec(
        during = list(
            mp_inflow("N", "r", "birth"),
            mp_per_capita_outflow("N", "d", "death")
        )
    )
    expect_equal(mp_state_vars(s1), "N")
    expect_equal(mp_flow_vars(s1), c("birth", "death"))
})

mp_tmb_model_spec(
    during = list(
        N ~ S + I
      , mp_per_capita_flow("S", "I", "beta * I / N", "infection")
      , mp_inflow("I", "mu", "importation")
    )
)

si = test_cache_read("SPEC-si.rds") |> mp_euler_multinomial()
si$change_model$update_flows
