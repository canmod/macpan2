test_that("round engine function computes correctly", {
  set.seed(1L)
  mp = simple_sims(
      iteration_exprs = list(y ~ rbinom(n, p))
    , time_steps = 1
    , mats = list(y = empty_matrix, n = c(10, 20), p = c(0.25, 0.01))
  )
  expect_snapshot(mp)
})
