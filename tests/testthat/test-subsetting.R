test_that("subsetting of matrices is _roughly_ similar to base R", {
  A = matrix(1:12, 4, 3)
  i = rep(0:3, 1:4)
  expect_equal(
    engine_eval(~ A[i, 0], A = A, i = i),
    matrix(A[i + 1, 1])
  )

  expect_error(
    engine_eval(~ A[0, ], A = A),
    "the expression given by"
  )
})

test_that("block subsetting is identical to square bracket subsetting", {
  A = matrix(1:12, 4, 3)
  expect_identical(
    engine_eval(~block(A, 1, 0, 2, 2), A = A),
    engine_eval(~A[1:2, 0:1], A = A)
  )
})

test_that("index bounds are checked", {
  x = 0.1 * (1:5)
  expect_error(
    engine_eval(~x[-1], x = x),
    "The following error was thrown by the TMB engine"
  )
  expect_error(
    engine_eval(~x[-5], x = x),
    "The following error was thrown by the TMB engine"
  )
  expect_equal(
    engine_eval(~x[0], x = x),
    matrix(0.1)
  )
})
