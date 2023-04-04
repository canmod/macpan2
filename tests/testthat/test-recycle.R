test_that("recycling is up to spec", {
  A = matrix(1:12, 4, 3)
  x = 1:3

  ## succeed in not recycling if it is not necessary
  expect_equal(
    engine_eval(~ recycle(A, 4, 3), A = A),
    A
  )

  ## recycle a constant to fill a matrix
  expect_equal(
    engine_eval(~ recycle(1, 4, 5)),
    matrix(1, 4, 5)
  )

  ## recycle a column vector to add matrix columns
  expect_equal(
    engine_eval(~ recycle(x, 3, 4), x = x),
    matrix(x, nrow = 3, ncol = 4)
  )

  ## fail to recycle a row vector to add matrix columns
  expect_error(
    engine_eval(~ recycle(t(x), 1, 2), x = x),
    "Error thrown by the TMB engine"
  )

  ## recycle a row vector to add matrix rows
  expect_equal(
    engine_eval(~ recycle(t(x), 4, 3), x = x),
    matrix(x, 4, 3, byrow = TRUE)
  )

  ## fail to recycle a column vector to add matrix rows
  expect_error(
    engine_eval(~ recycle(x, 2, 1), x = x),
    "Error thrown by the TMB engine"
  )

  ## fail to 'multiplicatively recycle'
  expect_error(
    engine_eval(~ recycle(x, 6, 6), x = x),
    "Error thrown by the TMB engine"
  )

  ## fail if one of the requested dimensions is just totally off,
  ## either multiplicatively or otherwise
  expect_error(
    engine_eval(~ recycle(A, 5, 3), A = A),
    "Error thrown by the TMB engine"
  )
  expect_error(
    engine_eval(~ recycle(A, 4, 4), A = A),
    "Error thrown by the TMB engine"
  )
})
