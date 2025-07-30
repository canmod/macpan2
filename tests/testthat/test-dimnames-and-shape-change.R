test_that("matrices with dimnames can change size", {
  m = macpan2:::TMBModel(
      init_mats = macpan2:::MatsList(x = c(a = 0), .mats_to_save = "x", .mats_to_return = "x"),
      expr_list =mp_tmb_expr_list(during = list(x ~ c(x, time_step(0)))),
      time_steps = macpan2:::Time(3)
  )
  s = macpan2:::TMBSimulator(m)
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
  m = macpan2:::TMBModel(
      init_mats = macpan2:::MatsList(x = empty_matrix
        , .mats_to_save = "x"
        , .mats_to_return = "x"
        , .dimnames = list(x = list(letters[1:2], ""))
      ),
      expr_list =mp_tmb_expr_list(during = list(x ~ c(x, 1))),
      time_steps = macpan2:::Time(3)
  )
  s = macpan2:::TMBSimulator(m)
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
