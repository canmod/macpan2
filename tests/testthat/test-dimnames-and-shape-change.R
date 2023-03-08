library(macpan2)
library(testthat)

test_that("matrices with dimnames can change size", {
  m = TMBModel(
      init_mats = MatsList(x = c(a = 0), .mats_to_save = "x", .mats_to_return = "x"),
      expr_list = ExprList(during = list(x ~ c(x, time_step(0)))),
      time_steps = Time(3)
  )
  s = TMBSimulator(m)
  r = s$report(.phases = c("before", "during"))
  expect_identical(
    r$value,
    sequence(0:4) - 1
  )
  expect_identical(
    r$row,
    c("a", 2:4)[sequence(0:4)]
  )
})

test_that("initially empty matrices can be associated with dimnames for at least some elements", {
  m = TMBModel(
      init_mats = MatsList(x = empty_matrix
        , .mats_to_save = "x"
        , .mats_to_return = "x"
        , .dimnames = list(x = list(letters[1:2], ""))
      ),
      expr_list = ExprList(during = list(x ~ c(x, 1))),
      time_steps = Time(3)
  )
  s = TMBSimulator(m)
  r = s$report(.phases = c("before", "during"))
  expect_identical(
    r$value,
    rep(1, 6)
  )
  expect_identical(
    r$row,
    c("a", "b", 3)[sequence(0:3)]
  )
})
