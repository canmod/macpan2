test_that("expression lists process literals", {
  m = macpan2:::TMBModel(
    init_mats = macpan2:::MatsList(a = 0),
    expr_list =mp_tmb_expr_list(before = list(a ~ 2020202)),
    params = macpan2:::OptParamsList(0, par_id = 0L, mat = "a", row_id = 0L, col_id = 0L),
    random = macpan2:::OptParamsList(),
    time_steps = macpan2:::Time(1L),
    obj_fn = macpan2:::ObjectiveFunction(~0)
  )
  expect_identical(m$data_arg()$literals, c(2020202, 0))
  
})
test_that("numbers in e-notation can safely get passed as literals", {
  expect_equal(
      engine_eval(~2.2e4 / 5.2e+3)
    , as.matrix(2.2e+4 / 5.2e3)
  )
  
  op = options(scipen = -8)
  x = macpan2:::ExprList(list(x ~ 1e2 + 2.3e-3 + 0.004e+3))$all_formula_vars()
  options(op)
  expect_identical(x, "x")
})
