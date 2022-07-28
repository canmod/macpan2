library(TMB)
library(macpan2)
library(dplyr)

eval_test_case = function(case_name) {
  try(with(valid_vars, eval(test_cases[[case_name]]$expr[[2]])))
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

# this first expression will work immediately, but doesn't include
# matrix operations:
expr = ~ beta * I / 100

# this second expression will not work until `c`, `matrix`, `sum`,
# and `%*%` are implemented:
expr = ~ sum(matrix(c(beta, I, beta, I), 2, 2) %*% c(1 / 2, 0.123))

test_cases = list(
  bilinear = list(
    expr = ~ c(beta, beta, I) %*% matrix(c(beta, beta, beta, beta, beta, beta), 3, 2) %*% c(I, I),
    expected = 181.2
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
  )
)

# compute value returned by the r parser for all test cases
sapply(names(test_cases), eval_test_case, simplify = FALSE)

# -----------------------------------
# CHOOSE TEST CASE
case_name = "dimensional_inconsistency_error"
# test case options are:
print(names(test_cases))
# -----------------------------------

expr = test_cases[[case_name]]$expr

parsed_expr = parser(expr)

parsed_expr$parse_table
parsed_expr$valid_literals	# could be empty list() in the case there is no literals in the expr
parsed_expr$valid_vars

data_args = list(
  parse_table_x = parsed_expr$parse_table$x,
  parse_table_n = parsed_expr$parse_table$n,
  parse_table_i = parsed_expr$parse_table$i,
  valid_literals = unlist(parsed_expr$valid_literals)
)
param_args = list(
  valid_vars = unlist(parsed_expr$valid_vars)
)

f = MakeADFun(
  data = data_args,
  parameters = param_args,
  DLL = 'structured_expressions'
)

# these two should be the same:
f$fn()
test_cases[[case_name]]$expected

# this should be the same too unless we want to deviate from the
# behaviour of the r parser
eval_test_case(case_name)

# this is the gradient of f$fn()
f$gr()
