test_that("time_step function returns a valid time-step no matter what", {
  m = macpan2:::TMBModel(
    init_mats = MatsList(x = 0, .mats_to_save = "x", .mats_to_return = "x"),
    expr_list =mp_tmb_expr_list(during = list(x ~ time_step(4))),
    time_steps = Time(10)
  )
  expect_identical(
    c(0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6),
    m$ad_fun()$report()$values[,5]
  )
  expect_error(
    simple_sims(list(x ~ time_step(-4)), 10, mats = list(x = 0)),
    regexp = "Time lag needs to be non-negative"
  )
})
