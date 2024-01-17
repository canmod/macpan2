test_that("concatenation works with many different shapes of input", {
  set.seed(1L)
  mats = MatsList(
      answer = empty_matrix
    , z = rnorm(4L)
    , w = matrix(rnorm(12L), 3L, 4L)
    , .mats_to_return = c("answer", "z", "w")
  )
  expr_good = ExprList(before = list(answer ~ cbind(z, t(w))))
  expr_bad = ExprList(before = list(answer ~ cbind(z, w)))
  good = TMBSimulator(TMBModel(mats, expr_good))
  bad = TMBSimulator(TMBModel(mats, expr_bad))

  answer = good$matrix(matrix_name = "answer", time_step = 1L, .phases = c("before", "during", "after"))
  z = good$matrix(matrix_name = "z", time_step = 1L, .phases = c("before", "during", "after"))
  w = good$matrix(matrix_name = "w", time_step = 1L, .phases = c("before", "during", "after"))

  expect_identical(answer, cbind(z, t(w)))
  expect_identical(bad$error_code(), 27L)

  expect_equal(
    rbind(0:3, 10 + 0:3),
    engine_eval(~ rbind(a, 10 + a), a = t(0:3))
  )
})
