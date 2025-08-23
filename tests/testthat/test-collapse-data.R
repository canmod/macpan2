## https://github.com/canmod/macpan2/issues/350
test_that("trajectories and coefficient tables can be collapsed", {
  op = options(
      macpan2_collapse_traj = TRUE
    , macpan2_collapse_coef = TRUE
    , macpan2_collapse_default = TRUE
  )
  
  spec = test_cache_read("SPEC-sir.rds")
  sim = mp_simulator(spec, 50, c("infection", "recovery"))
  data = mp_trajectory(sim)
  expect_identical(names(data), c("variable", "time", "value"))
  cal = mp_tmb_calibrator(
      spec
    , data
    , traj = "infection"
    , par = c("beta", "gamma")
    , default = list(beta = 0.25, gamma = 0.25)
  )
  mp_optimize(cal)
  coef = mp_tmb_coef(cal, conf.int = TRUE)
  expect_identical(
      names(coef)
    , c("mat", "default", "estimate", "std.error", "conf.low", "conf.high")
  )
  spec_default = mp_default(spec)
  sim_default = mp_default(sim)
  expect_identical(spec_default, sim_default)
  expect_identical(
      names(spec_default)
    , c("quantity", "value")
  )
  
  options(op)
})
