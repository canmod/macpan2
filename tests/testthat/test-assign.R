library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("assignment indices are checked", {
  x = rep(1, 5)
  y = replace(x, c(2, 3), c(100, 200))
  expect_equal(
    engine_eval(~assign(x, c(1, 2), 0, c(100, 200))
      , x = x
      , .matrix_to_return = "x"
    ),
    matrix(y)
  )
  expect_error(
    engine_eval(~assign(x, c(1, 5), 0, c(100, 200))
      , x = x
      , .matrix_to_return = "x"
    ),
    "The following error was thrown by the TMB engine"
  )
})

simple_sims(
    list(
        c(x, y) ~ c(a, b)
    )
  , time_steps = 1L
  , mats = list(a = 1:3, b = 5:6, x = empty_matrix, y = empty_matrix)
)
