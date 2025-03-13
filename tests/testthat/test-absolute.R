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
