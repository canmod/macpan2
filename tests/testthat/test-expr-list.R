# test_that("ExprList returns valid expression lists for TMB", {
#
# })

test_that("expressions are inserted and printed", {
  l =mp_tmb_expr_list(during = list(a ~ 1, b ~ 2, c ~ 3))
  l = l$insert(X ~ 500, Y ~ 600, .phase = "before")
  l = l$insert(A ~ 0, .phase = "after")
  expect_snapshot(l$print_exprs())
  #"---------------------\\nBefore the simulation loop \\(t = 0\\):\\n---------------------\\n1: X ~ 500\\n2: Y ~ 600\\n\\n---------------------\\nAt every iteration of the simulation loop \\(t = 1 to T\\):\\n---------------------\\n1: a ~ 1\\n2: b ~ 2\\n3: c ~ 3\\n\\n---------------------\\nAfter the simulation loop \\(t = T\\):\\n---------------------\\n1: A ~ 0\\n"
})

test_that("formula validity is enforced", {
  # expect_error(
  #   macpan2:::TMBModel(
  #     init_mats = macpan2:::MatsList(a = 1),
  #     expr_list =mp_tmb_expr_list(before = list(a[0] ~ 1))
  #   ),
  #   "without subsetting on the left-hand-side"
  # )
  expect_error(
    macpan2:::TMBModel(
      init_mats = macpan2:::MatsList(a = 1),
      expr_list = mp_tmb_expr_list(before = list( ~ 1))
    ),
    regexp = "not all formulas are two-sided"
  )
})

test_that("proper error message comes when output matrices are not initialized", {
  m = macpan2:::TMBModel(
    init_mats = macpan2:::MatsList(a = 1),
    expr_list =mp_tmb_expr_list(before = list(b ~ a))
  )
  expect_error(
    m$data_arg(),
    "The expression given by"
  )
})
