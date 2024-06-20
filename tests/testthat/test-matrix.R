test_that("matrices can be made on the fly in the engine", {
  set.seed(1L)
  A = matrix(rnorm(6, 1:6, 1), 3, 2)
  x = rnorm(24)
  set.seed(1L)
  expect_identical(
    engine_eval(~ matrix(rnorm(1:6, 1), 3, 2)),
    A
  )
  expect_identical(
    engine_eval(~matrix(x, 12, 2), x = matrix(x, 3, 8)),
    matrix(x, 12, 2)
  )
  expect_error(
    engine_eval(~matrix(x, 12, 2), x = matrix(1:35, 7, 5)),
    regexp = "The size of the input must be less than or equal to that of the output"
  )
  expect_equal(
    engine_eval(~matrix(1/2, 2, 3)),
    matrix(1/2, 2, 3)
  )
  y = (1:3) / (4:6)
  expect_equal(
    engine_eval(~matrix(y, 2, 3), y = y),
    matrix(y, 2, 3)
  )
  expect_error(
    engine_eval(~ matrix(c(1,2,3,4), 2, 2, byrow = TRUE)),
    regexp = "Too many arguments provided to function. Note this function differs from the base R version in the arguments it accepts."
  )
})
