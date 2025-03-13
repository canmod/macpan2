test_that("vectors and vector elements cannot have the same name.", {
  spec = mp_tmb_model_spec(default = list(a = 0, b = c(a = 0)))
  expect_error(
      mp_tmb_calibrator(spec, empty_trajectory)
    , "The following names were used for one or more purposes"
  )
})
test_that("empty specs lead to calibrators with empty trajectories.", {
  spec = mp_tmb_model_spec()
  cal = mp_tmb_calibrator(spec, empty_trajectory)
  expect_equal(mp_trajectory(cal), empty_trajectory)
})
