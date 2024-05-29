test_that("mean and sd are correct", {
  set.seed(1L)
  x = rnorm(10)
  expect_equal(matrix(mean(x)), engine_eval(~mean(x), x = x))
  expect_equal(matrix(sd(x)), engine_eval(~sd(x), x = x))
})
