test_that("simulator method works", {
  expect_identical(
    macpan2:::TMBModel(
      init_mats = macpan2:::MatsList(x = empty_matrix),
      expr_list =mp_tmb_expr_list(before = list(x ~ 1)),
    )$simulator()$report(),
    structure(
      list(
        matrix = character(0),
        time = integer(0),
        row = numeric(0),
        col = numeric(0),
        value = numeric(0)
      ),
      row.names = integer(0), class = "data.frame"
    )
  )

  null_model = macpan2:::TMBModel()
  null_model$init_mats$.names()
})
