test_that("piecewise time variation estimates are constant if appropriate", {
  spec = test_cache_read("SPEC-sir.rds")
  sim = mp_simulator(spec, 50, "infection")
  data = mp_trajectory(sim)
  tv = data.frame(
      matrix = "beta"
    , time = c(10, 25)
    , row = 0, col = 0
    , value = c(0.1, 0.5)
  )
  cal = mp_tmb_calibrator(
      spec
    , data = bind_rows(data, tv)
    , traj = "infection"
    , tv = "beta"
    , par = "beta"
    , default = list(beta = 0.25)
  )
  mp_optimize(cal)
  cc = mp_tmb_coef(cal)[,c("mat", "row", "default", "estimate")]
  cc$estimate = round(cc$estimate, 5)
  
  expect_equal(
    cc, 
    structure(list(mat = c("time_var_beta", "time_var_beta", "time_var_beta"
    ), row = 0:2, default = c(0.25, 0.1, 0.5), estimate = c(0.2, 
    0.2, 0.2)), row.names = c(NA, -3L), class = "data.frame")
  )
})

test_that("simulations can end before the last change point", {
  spec = (test_cache_read("SPEC-si.rds")
    |> mp_tmb_insert(
        expressions = list(beta ~ time_var(beta_values, beta_times))
      , default = list(beta_values = c(0.3, 0.2, 0.1))
      , integers = list(beta_times = c(0,   5 , 10))
    )
  )
  spec_conservative = mp_tmb_update(spec
    , default = list(beta_values = c(0.3, 0.2))
    , integers = list(beta_times = c(0,   5)) 
  )
  mk_sim = function(spec) {
    spec |> mp_simulator(7, "infection") |> mp_trajectory() |> pull(value)
  }
  expect_equal(mk_sim(spec), mk_sim(spec_conservative))
})
