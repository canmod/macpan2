options(macpan2_verbose = FALSE)
spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
test_that("default/initial values in specs/sims are consistent", {
  updates = list(N = 10000, beta = 0.3)
  
  sim_zero = mp_simulator(spec, 0L, "I")
  sim_zero_update = mp_simulator(spec, 0L, "I", updates)
  
  spec_defaults = arrange(mp_default(spec), matrix)
  sim_defaults = arrange(mp_default(sim_zero), matrix)
  sim_updates = arrange(mp_default(sim_zero_update), matrix)
  
  expect_equal(spec_defaults, sim_defaults)
  expect_equal(
    with(sim_updates, setNames(value, matrix))[names(updates)],
    unlist(updates)
  )
  
  initial_spec = mp_initial(spec)
  initial_sim = mp_initial(sim_zero)
  initial_list_spec = mp_initial_list(spec)
  initial_list_sim = mp_initial_list(sim_zero)
  
  expect_equal(
      initial_spec[order(initial_spec$matrix), , drop = FALSE]
    , initial_sim[order(initial_sim$matrix), , drop = FALSE]
    , ignore_attr = TRUE
  )
  expect_setequal(names(initial_list_spec), names(initial_list_sim))
  expect_equal(initial_list_spec[names(initial_list_sim)], initial_list_sim)
})

test_that("optimized calibration impacts initial values properly", {
  spec_with_R0 = (spec
    |> mp_tmb_insert("before"
        , at = Inf
        , expressions = R0 ~ beta/gamma
    )
  )
  sim = mp_simulator(spec_with_R0, 50, "infection")
  data = mp_trajectory(sim)
  cal = mp_tmb_calibrator(
      spec_with_R0
    , data
    , traj = "infection"
    , par = c("beta", "gamma")
    , default = list(beta = 0.25, gamma = 0.17)
  )
  
  expect_equal(mp_initial_list(cal)$R0[,], 0.25/0.17)
  mp_optimize(cal)
  
  expect_equal(mp_initial_list(spec_with_R0)$R0, mp_initial_list(cal)$R0)

})

test_that("there is control over whether unused defaults are returned", {
  spec_with_unused = mp_tmb_update(spec, default = list(unused = 0))

  expect_false("unused" %in% names(mp_default_list(spec_with_unused)))
  expect_true("unused" %in% names(mp_default_list(spec_with_unused, include_all = TRUE)))
  
  sim = mp_simulator(spec_with_unused, 0L, "infection")
  expect_false("unused" %in% names(mp_default_list(sim)))
  expect_true("unused" %in% names(mp_default_list(sim, include_all = TRUE)))
  
  data = mp_trajectory(sim)
  cal = mp_tmb_calibrator(spec_with_unused, data)
  
  expect_false("unused" %in% names(mp_default_list(cal)))
  expect_true("unused" %in% names(mp_default_list(cal, include_all = TRUE)))
})
