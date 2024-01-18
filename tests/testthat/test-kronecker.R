test_that("kronecker product works", {
  x = matrix(1:12, 3, 4)
  y = matrix(1:8, 4, 2)
  expect_equal(
    engine_eval(~ x %x% y, x = x, y = y),
    x %x% y
  )
})
