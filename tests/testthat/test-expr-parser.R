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

  f = make_expr_parser("f", finalizer = finalizer_char)
  g = make_expr_parser("g", finalizer = finalizer_index)

  ## work around testthat calling stuff from elsewhere.
  ## the issue is that make_expr_parser assumes that you are going to be
  ## calling from the same environment (or at least from an environment
  ## that can reach the environment in which the function was made) -- it
  ## is a recursive function. the idea of make_expr_parser is that it gets
  ## used at package loading time to create macpan2:::parse_expr, which gets
  ## used only by package functions. this ensures that it will be in the macpan2
  ## namespace, which can be easily accessed by other functions in the
  ## namespace. here we need to contrive an unrealistic case so that the
  ## test and coverage infrastructure works.
  assign("f", f, envir = .GlobalEnv)
  assign("g", g, envir = .GlobalEnv)

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
