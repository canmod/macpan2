test_that("vector repeating is correct", {
  set.seed(1L)
  A = matrix(rnorm(6, 1:6, 1), 3, 2)
  x = rnorm(24)
  expect_identical(
    engine_eval(~ rep(y, 3), y = x),
    matrix(rep(x, 3))
  )
  expect_identical(
    engine_eval(~ rep(y, 3), y = A),
    matrix(rep(A, 3))
  )
})
