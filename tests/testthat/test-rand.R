test_that("random number generators handle not-enough parameter errors", {
  expect_error(engine_eval(~rnorm(0)), "The following error was thrown by the TMB engine")
  expect_error(engine_eval(~rnbinom(0)), "The following error was thrown by the TMB engine")
})

test_that("negative binomial simulation respects seeds and math", {
  set.seed(1L)
  x = matrix(rnbinom(100, 10, mu = 2))
  set.seed(1L)
  y = engine_eval(~rnbinom(rep(2, 100), 10))
  expect_equal(x, y)
})

test_that("poisson simulation respects seeds and math", {
  set.seed(1L)
  x = matrix(rpois(100, 2))
  set.seed(1L)
  y = engine_eval(~rpois(rep(2, 100)))
  expect_equal(x, y)
})
