test_that("default values in specs and sims are consistent", {
  spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
  sim = mp_simulator(spec, 0L, "I")
  updates = list(N = 10000, beta = 0.3)
  spec_defaults = mp_default(spec)
  sim_defaults = mp_default(sim)
  sim_updates = mp_default(mp_simulator(spec, 0L, "I", updates))
  
  expect_equal(spec_defaults, sim_defaults)
  expect_equal(
    with(sim_updates, setNames(value, matrix))[names(updates)],
    unlist(updates)
  )
})
