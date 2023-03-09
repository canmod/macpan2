test_that("matrix multiplication works", {
  set.seed(1L)
  A = matrix(rnorm(12), 4, 3)
  x = rnorm(3)
  expect_equal(
    engine_eval(~ A %*% x, A = A, x = x),
    A %*% x
  )
})
