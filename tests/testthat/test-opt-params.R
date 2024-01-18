test_that("parameters are not initially empty", {
  params = OptParamsList(0
    , par_id = 0L
    , mat = "x"
    , row_id = 0L
    , col_id = 0L
  )
  init_mats = MatsList(
      x = empty_matrix
    , y = empty_matrix
    , .mats_to_save = "y"
    , .mats_to_return = "y"
  )
  expr_list = ExprList(before = list(y ~ x + 1))
  expect_error(
    TMBModel(
      init_mats = init_mats,
      expr_list = expr_list,
      params = params
    ), 
    regexp = "optimization parameters are not consistent with matrices"
  )
})
