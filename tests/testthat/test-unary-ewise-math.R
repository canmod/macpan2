test_that("logs and exps are correct", {
  set.seed(1L)
  x = rnorm(10)
  y = abs(x)
  expect_equal(matrix(exp(x)), engine_eval(~exp(x), x = x))
  expect_equal(matrix(log(y)), engine_eval(~log(y), y = y))
})
