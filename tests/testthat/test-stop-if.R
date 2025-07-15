test_that("simulations can be stopped early", {
  exprs = list(x ~ x - 2, sims = dummy ~ stop_if_lt(x), y ~ y - 2)
  def = list(x = 10, y = 10)
  ts = 10
  spec = mp_tmb_model_spec(during = exprs, default = def)
  sim = mp_simulator(spec, ts, c("x", "y"))
  
  expect_equal(
      mp_final(sim)$value
    , c(-2, 0)
  )
  expect_equal(
      mp_trajectory(sim)$value
    , rep(seq(8, 0, by = -2), each = 2)
  )
})
