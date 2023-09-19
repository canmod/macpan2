test_that("simulator method works", {
  expect_identical(
    TMBModel(
      init_mats = MatsList(x = empty_matrix),
      expr_list = ExprList(before = list(x ~ 1)),
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

  null_model = TMBModel()
  null_model$init_mats$.names()
})
