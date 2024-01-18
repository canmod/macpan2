test_that("diagonals are properly set and extracted", {
  A = matrix(1:9, 3, 3)
  b = diag(A)
  B = diag(b)
  expect_equal(
    engine_eval(~from_diag(A), A = A),
    matrix(diag(A))
  )
  expect_equal(
    engine_eval(~to_diag(from_diag(A)), A = A),
    B
  )
})
