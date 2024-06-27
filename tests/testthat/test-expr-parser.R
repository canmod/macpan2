test_that("missing variables in expressions are handled", {
  expect_error(engine_eval(~x), "The expression given by")
})

test_that("parse_tables ...", {
  eval_env = list2env(list(
    valid_funcs = macpan2:::valid_funcs,
    valid_vars = list(x = 1, y = 2)
  ))
  form = ~ log(x) + exp(y)
  environment(form) = eval_env

  f = make_expr_parser(finalizer = finalizer_char)
  g = make_expr_parser(finalizer = finalizer_index)

  g_table = g(form)
  f_table = f(form)

  expect_identical(
    g_table$parse_table[c("x", "n", "i")],
    data.frame(
        x = c(0L, 6L, 5L, 0L, 1L)
      , n = c(2L, 1L, 1L, 0L, 0L)
      , i = c(1L, 3L, 4L, 0L, 0L)
    )
  )
  expect_identical(
    f_table,
    data.frame(
      x = c("~", "+", "log", "exp", "x", "y"),
      n = c(1L, 2L, 1L, 1L, 0L, 0L),
      i = c(1L, 2L, 4L, 5L, 0L, 0L)
    )
  )
})
