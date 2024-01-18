test_that("expression lists process literals", {
  m = macpan2:::TMBModel(
    init_mats = macpan2:::MatsList(a = 0),
    expr_list =mp_tmb_expr_list(before = list(a ~ 2020202)),
    params = macpan2:::OptParamsList(0, par_id = 0L, mat = "a", row_id = 0L, col_id = 0L),
    random = macpan2:::OptParamsList(),
    time_steps = macpan2:::Time(1L),
    obj_fn = macpan2:::ObjectiveFunction(~0)
  )
  testthat::expect_identical(m$data_arg()$literals, c(2020202, 0))
})
