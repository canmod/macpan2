library(TMB)
library(macpan2)
library(dplyr)

gen_dim = function(x) {
  d = dim(x)
  if (is.null(d)) return(c(length(x), 1L))
  if (length(d) != 2L) stop("only matrices, vectors, and scalars are allowed")
  d
}

element_wise = function(f = base::`*`, e1, e2) {
  d1 = gen_dim(e1)
  d2 = gen_dim(e2)
  is_equal_dims = d1 == d2
  if (all(is_equal_dims)) return(f(e1, e2))
  d = pmax(d1, d2)
  is_dim_1 = pmin(d1, d2) == 1L
  all(is_dim_1 | is_equal_dims)
  if (!is_equal_dims[1]) {

  }
}

stretch = function(x, row_times, col_times) {
  x = as.matrix(x)
  x =   matrix(rep(c(  x ), times = col_times), nrow(x), col_times * ncol(x))
  x = t(matrix(rep(c(t(x)), times = row_times), ncol(x), row_times * nrow(x)))
  x
}
#stretch(c(1, 3), 3, 4)

compile('structured_expressions.cpp')
dyn.load(dynlib("structured_expressions"))

parser = make_expr_parser('parser', finalizer_index)
valid_funcs = nlist(
  `+`,
  `-`,
  `*`,
  `/`,
  `^`,
  `(`,
  `c`,
  `matrix`,
  `%*%`,
  `sum`,
  `rep`    # rep(beta, 3) == c(beta, beta, beta)
)

valid_vars = list(beta = 0.1, I = 30)

test_cases = list(
  really_simple = list(
    expr = ~ beta * 1,
    expected = matrix(0.1, 1, 1)
  ),
  guan_test = list(
    expr = ~
      matrix(3.2*c(beta, beta, I) - 2*rep(beta, 3), 1, 3) %*%
      (
        matrix(rep(beta, 6), 3, 2) -
        matrix(rep(5.0, 3), 3, 1)
      ) %*%
      matrix(c(I, I), 2, 1),
    expected = matrix(-28235.76, 1, 1)
  ),
  bilinear = list(
    expr = ~ c(beta, beta, I) %*% matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) %*% c(I, I),
    expected = "ERROR" # not 181.2
  ),
  bilinear_with_matrices = list(
    expr = ~ matrix(c(beta, beta, I), 1, 3) %*% matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) %*% matrix(c(I, I), 2, 1),
    expected = matrix(181.2, 1, 1)
  ),
  rep_function = list(
    expr = ~ sum(rep(beta, 3)),
    expected = matrix(0.3, 1, 1)
  ),
  dimensional_inconsistency_error = list(
    expr = ~ matrix(c(beta, I), 2, 1) %*% matrix(c(beta, I, beta, I), 2, 2),
    expected = "ERROR"
  ),
  scalar_times_vector = list(
    expr = ~ sum(I * c(0.5, beta)),
    expected = matrix(18, 1, 1)
  ),
  scalar_times_matrix = list(
    expr = ~ sum(I * matrix(c(0.5, beta, I, 0.6), 2, 2)),
    expected = matrix(936, 1, 1)
  ),
  no_literals = list(
    expr = ~ I,
    expected = matrix(30, 1, 1)
  ),
  row_times_column = list(
    expr = ~ sum(matrix(c(1, beta), 1, 2) * matrix(c(1, beta), 2, 1)),
    expected = "ERROR"
  ),
  row_times_vector = list(
    expr = ~ sum(matrix(c(1, beta), 1, 2) * c(1, beta)),
    expected = "ERROR"
  ),
  linear = list(
    expr = ~ matrix(rep(beta, 6), 3, 2) %*% rep(I, 2),
    expected = matrix(6, 3, 1)
  )
)

eval_test_case_with_r_parser = function(test_case) {
  out = try(with(valid_vars, eval(test_case$expr[[2]])))
  if (isTRUE(class(out) == 'try-error')) return("ERROR")
  out
}

eval_test_case_with_cpp_parser = function(test_case) {
  expr = test_case$expr

  parsed_expr = try(parser(expr))
  if (class(parsed_expr) == "try-error") return("ERROR")

  data_args = list(
    parse_table_x = parsed_expr$parse_table$x,
    parse_table_n = parsed_expr$parse_table$n,
    parse_table_i = parsed_expr$parse_table$i,
    valid_literals = as.numeric(unlist(parsed_expr$valid_literals))
  )
  param_args = list(
    valid_vars = unlist(parsed_expr$valid_vars)
  )

  f = try(MakeADFun(
    data = data_args,
    parameters = param_args,
    DLL = 'structured_expressions'
  ))

  if (class(f) == 'try-error') stop("ERROR")

  if (f$report()$error_code != 0L) return("ERROR")
  f$report()$result
}

evaluate_test_case = function(
    test_case,
    return_option = c("compare_with_expectations", "compare_with_r_parser", "values")
  ) {
  return_option = match.arg(return_option)


  # this is the gradient of f$fn()
  #f$gr()

  print(cpp_answer <- try(eval_test_case_with_cpp_parser(test_case)))
  print(exp_answer <- test_case$expected)
  print(r_answer <- eval_test_case_with_r_parser(test_case))

  if (return_option == 'compare_with_r_parser') {
    return(isTRUE(all.equal(cpp_answer, r_answer)))
  } else if (return_option == 'compare_with_expectations') {
    return(isTRUE(all.equal(cpp_answer, exp_answer)))
  }
  nlist(cpp_answer, exp_answer, r_answer)
}

outputs = lapply(test_cases, evaluate_test_case, "values")
data.frame(
  r_parser = unlist(lapply(test_cases, evaluate_test_case, "compare_with_r_parser")),
  expectations = unlist(lapply(test_cases, evaluate_test_case, "compare_with_expectations")),
  expect_error = unlist(lapply(lapply(lapply(test_cases, getElement, 'expected'), `all.equal`, "ERROR"), isTRUE))
)


# rough notes on how to handle matrix-valued valid_vars:
#
# valid_vars = list(A = matrix(1, 3, 2))
# ex = ~ sum(A)
# data.frame(
#   params = c(1.4, 0.2),
#   var_id = c(1, 1),
#   row_id = c(1, 3),
#   col_id = c(2, 1)
# )
# valid_vars[[1]][1, 2] = 1.4
# valid_vars[[1]][3, 1] = 0.2
# evaluate(ex)

