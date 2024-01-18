test_that("expression lists process literals", {
  m = macpan2:::TMBModel(
    init_mats = MatsList(a = 0),
    expr_list =mp_tmb_expr_list(before = list(a ~ 2020202)),
    params = OptParamsList(0, par_id = 0L, mat = "a", row_id = 0L, col_id = 0L),
    random = OptParamsList(),
    time_steps = Time(1L),
    obj_fn = ObjectiveFunction(~0)
  )
  testthat::expect_identical(m$data_arg()$literals, c(2020202, 0))
})
