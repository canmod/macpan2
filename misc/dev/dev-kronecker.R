library(macpan2)
library(TMB)

test_that("kroncker product works", {
  x = matrix(1:4, 2, 2)
  y = matrix(1:12, 3, 4)
  expect_equal(
    engine_eval(~x %x% y, x = x, y = y),
    x %x% y
  )
})
