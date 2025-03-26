test_that("density functions handle not-enough parameter errors", {
  expect_error(engine_eval(~dpois(0)), "The following error was thrown by the TMB engine")
  expect_error(engine_eval(~dnorm(0)), "The following error was thrown by the TMB engine")
  expect_error(engine_eval(~dnbinom(0)), "The following error was thrown by the TMB engine")
})

test_that("normal densities respect math", {
  set.seed(1L)
  z = rnorm(100, 10, 2)
  x = sum(matrix(dnorm(z, 10, 2, log = TRUE)))
  y = sum(engine_eval(~dnorm(z, rep(10, 100), 2), z = z))
  expect_equal(x, y)
})

test_that("negative binomial densities respect math", {
  set.seed(1L)
  z = rnbinom(100, 10, mu = 2)
  x = sum(matrix(dnbinom(z, 10, mu = 2, log = TRUE)))
  y = sum(engine_eval(~dnbinom(z, rep(2, 100), 10), z = z))
  expect_equal(x, y)
})

test_that("poisson densities respects math", {
  set.seed(1L)
  z = rpois(100, 10)
  x = sum(matrix(dpois(z, 10, log = TRUE)))
  y = sum(engine_eval(~dpois(z, rep(10, 100)), z = z))
  expect_equal(x, y)
  
})

test_that("bad recycling in density functions get errors", {
  expect_error(
      engine_eval(~dnorm(x, y, 1), x = rnorm(2), y = rnorm(3)),
      regexp = "cannot recycle rows"
  )
})

test_that("missing standard deviation is flagged in error message", {
  expect_error(
      engine_eval(~dnorm(1, 0)),
      regexp = "dnorm needs three arguments"
  )
})

test_that("basic dbinom engine tests", {
  set.seed(1L)
  sizevec = rep(1:5, each = 20)
  probvec = rep(1:5 / 10, each = 20)
  z = rbinom(100, size = sizevec, prob = probvec)
  x = sum(dbinom(z, prob = probvec, size = sizevec, log = TRUE))
  y = sum(engine_eval(~dbinom(z, sizevec, probvec),
                      z = z, sizevec = sizevec, probvec = probvec))
  expect_equal(x, y)

})
