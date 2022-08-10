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
  guan_test = list(
    expr = ~ matrix(3.2*c(beta, beta, I) - 2*rep(beta, 3), 1, 3) %*% (matrix(c(beta, beta, beta, beta, beta, beta), 3, 2)-matrix(rep(5.0, 3), 3, 1))  %*% matrix(c(I, I), 2, 1),
    expected = -28235.76
  ),
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
    expected = 1.01
  )
)

# compute value returned by the r parser for all test cases
sapply(names(test_cases), eval_test_case, simplify = FALSE)

# -----------------------------------
# CHOOSE TEST CASE
case_name = "guan_test"
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
