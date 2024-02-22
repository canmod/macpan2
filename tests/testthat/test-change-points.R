test_that("change pointers can be incremented", {
  sims = simple_sims(
    iteration_exprs = list(pointer ~ time_group(pointer, change_points)),
    time_steps = 10,
    mats = list(
      pointer = 0,
      change_points = c(0, 4, 7)
    )
  )
  expect_equal(
    sims[sims$matrix == "pointer", "value"],
    c(0, 0, 0, 1, 1, 1, 2, 2, 2, 2)
  )
})
