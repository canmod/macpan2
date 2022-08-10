library(TMB)
library(macpan2)
library(dplyr)

eval_test_case_with_r_parser = function(test_case) {
  out = try(with(valid_vars, eval(test_case$expr[[2]])))
  if (class(out) == 'try-error') return("ERROR")
  out
}

eval_test_case_with_cpp_parser = function(test_case) {
  expr = test_case$expr

  parsed_expr = try(parser(expr))
  if (class(parsed_expr) == "try-error") return("ERROR")

  # parsed_expr$parse_table
  # parsed_expr$valid_literals	# could be empty list() in the case there is no literals in the expr
  # parsed_expr$valid_vars

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
  f$fn()
}

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
    expected = 0.1
  ),
  guan_test = list(
    expr = ~
      matrix(3.2*c(beta, beta, I) - 2*rep(beta, 3), 1, 3) %*%
      (
        matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) -
        matrix(rep(5.0, 3), 3, 1)
      ) %*%
      matrix(c(I, I), 2, 1),
    expected = -28235.76
  ),
  bilinear = list(
    expr = ~ c(beta, beta, I) %*% matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) %*% c(I, I),
    expected = "ERROR" # not 181.2
  ),
  bilinear_with_matrices = list(
    expr = ~ matrix(c(beta, beta, I), 1, 3) %*% matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) %*% matrix(c(I, I), 2, 1),
    expected = 181.2
  ),
  rep_function = list(
    expr = ~ sum(rep(beta, 3)),
    expected = 0.3
  ),
  dimensional_inconsistency_error = list(
    expr = ~ matrix(c(beta, I), 2, 1) %*% matrix(c(beta, I, beta, I), 2, 2),
    expected = "ERROR"
  ),
  scalar_times_vector = list(
    expr = ~ sum(I * c(0.5, beta)),
    expected = 18
  ),
  scalar_times_matrix = list(
    expr = ~ sum(I * matrix(c(0.5, beta, I, 0.6), 2, 2)),
    expected = 936
  ),
  no_literals = list(
    expr = ~ I,
    expected = 30
  ),
  row_times_column = list(
    expr = ~ sum(matrix(c(1, beta), 1, 2) * matrix(c(1, beta), 2, 1)),
    expected = "ERROR"
  ),
  row_times_vector = list(
    expr = ~ sum(matrix(c(1, beta), 1, 2) * c(1, beta)),
    expected = "ERROR"
  )
)

# # compute value returned by the r parser for all test cases
# sapply(names(test_cases), eval_test_case_with_r_parser, simplify = FALSE)

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
  expectations = unlist(lapply(test_cases, evaluate_test_case, "compare_with_expectations"))
)
