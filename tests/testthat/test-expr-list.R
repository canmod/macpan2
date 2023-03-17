# test_that("ExprList returns valid expression lists for TMB", {
#
# })

test_that("expressions are inserted and printed", {
  l = ExprList(during = list(a ~ 1, b ~ 2, c ~ 3))
  expect_output(
    (l
     $insert(X ~ 500, Y ~ 600, .phase = "before")
     $insert(A ~ 0, .phase = "after")
     $print_exprs()
    ),
    "---------------------\\nBefore the simulation loop \\(t = 0\\):\\n---------------------\\n1: X ~ 500\\n2: Y ~ 600\\n\\n---------------------\\nAt every iteration of the simulation loop \\(t = 1 to T\\):\\n---------------------\\n1: a ~ 1\\n2: b ~ 2\\n3: c ~ 3\\n\\n---------------------\\nAfter the simulation loop \\(t = T\\):\\n---------------------\\n1: A ~ 0\\n"
  )
})

test_that("formula validity is enforced", {
  expect_error(
    TMBModel(
      init_mats = MatsList(a = 1),
      expr_list = ExprList(before = list(a[0] ~ 1))
    ),
    "without subsetting on the left-hand-side"
  )
  expect_error(
    TMBModel(
      init_mats = MatsList(a = 1),
      expr_list = ExprList(before = list( ~ 1))
    ),
    "Model expressions must be two-sided"
  )
})
