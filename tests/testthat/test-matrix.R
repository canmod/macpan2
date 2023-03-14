test_that("matrices can be made on the fly in the engine", {
  set.seed(1L)
  A = matrix(rnorm(6, 1:6, 1), 3, 2)
  set.seed(1L)
  expect_identical(
    engine_eval(~ matrix(rnorm(1:6, 1), 3, 2)),
    A
  )
})
