test_that("test that parentheses work", {
  set.seed(1L)
  x = runif(1L)
  expect_equal(matrix(x * (1 - x)), engine_eval(~ x * (1 - x), x = x))
})
