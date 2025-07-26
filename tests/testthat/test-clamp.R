library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("clamping function is modified squareplus", {
  x = seq(-1e-11, 1e-11, by = 1e-14)
  clamp_base_r = function(x, eps = 1e-12, limit = eps) {
    limit + (
        (
            x - limit +
            sqrt(
                (x - limit)^2 +
                (2 * eps - limit)^2 -
                limit^2
            )
        ) / 2
    )
  }
  clamp_macpan = function(x) engine_eval(~clamp(x), x = x) |> c()
  expect_equal(clamp_base_r(x), clamp_macpan(x))
})

engine_eval(~divide_safe(1e-9, 1e-9, 0, 1e-12))
