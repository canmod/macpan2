test_that("round engine function computes correctly", {
  set.seed(1L)
  mp = simple_sims(
      iteration_exprs = list(y ~ round(x))
    , time_steps = 1
    , mats = list(y = empty_matrix, x = matrix(rnorm(153), 17, 9))
  )
  expect_equal(
    round(mp$value[mp$matrix == "x"]),
    mp$value[mp$matrix == "y"]
  )
})
