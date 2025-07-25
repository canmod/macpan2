test_that("parameter uncertainty is used by mp_trajectory_replicate", {
  cal = si_example_object("optimized_calibrator")
  mp_trajectory(cal)
  mp_trajectory_par(cal)
  comparison = all.equal(
      mp_trajectory_replicate(cal, 1)
    , mp_trajectory_replicate(cal, 1)
  )
  expect_false(isTRUE(comparison))
})
