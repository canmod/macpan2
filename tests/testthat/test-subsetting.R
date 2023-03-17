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
