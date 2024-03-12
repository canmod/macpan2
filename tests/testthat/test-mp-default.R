test_that("default values in specs and sims are consistent", {
  spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim = mp_simulator(spec, 0L, "I")
  updates = list(N = 10000, beta = 0.3)
  spec_defaults = arrange(mp_default(spec), matrix)
  sim_defaults = arrange(mp_default(sim), matrix)
  sim_updates = arrange(mp_default(mp_simulator(spec, 0L, "I", updates)), matrix)
  
  expect_equal(spec_defaults, sim_defaults)
  expect_equal(
    with(sim_updates, setNames(value, matrix))[names(updates)],
    unlist(updates)
  )
})
