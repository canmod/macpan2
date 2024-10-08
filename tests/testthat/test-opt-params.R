test_that("parameters are not initially empty", {
  params = macpan2:::OptParamsList(0
    , par_id = 0L
    , mat = "x"
    , row_id = 0L
    , col_id = 0L
  )
  init_mats = macpan2:::MatsList(
      x = empty_matrix
    , y = empty_matrix
    , .mats_to_save = "y"
    , .mats_to_return = "y"
  )
  expr_list =mp_tmb_expr_list(before = list(y ~ x + 1))
  expect_error(
    macpan2:::TMBModel(
      init_mats = init_mats,
      expr_list = expr_list,
      params = params
    ), 
    regexp = "optimization parameters are not consistent with matrices"
  )
})
