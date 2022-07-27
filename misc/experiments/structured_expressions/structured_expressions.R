library(TMB)
library(macpan2)
library(dplyr)
compile('structured_expressions.cpp')
dyn.load(dynlib("structured_expressions"))

parser = make_expr_parser('parser', finalizer_index)
valid_funcs = nlist(`+`, `-`, `*`, `/`, `^`, `(`, `c`, `matrix`, `%*%`, `sum`)
valid_vars = list(beta = 0.1, I = 30)

# this first expression will work immediately, but doesn't include
# matrix operations:
#expr = ~ beta * I / 100

# this second expression will not work until `c`, `matrix`, `sum`,
# and `%*%` are implemented:
expr = ~ sum(matrix(c(beta, I, beta, I), 2, 2) %*% c(1 / 2, 0.123))

parsed_expr = parser(expr)

parsed_expr$parse_table
parsed_expr$valid_literals
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
with(valid_vars, eval(expr[[2]]))

# this is the gradient of f$fn()
f$gr()

