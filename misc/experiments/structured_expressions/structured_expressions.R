library(TMB)
library(macpan2)
library(dplyr)
compile('structured_expressions.cpp')
dyn.load(dynlib("structured_expressions"))

parser = make_expr_parser('parser', finalizer_index)
valid_funcs = nlist(`+`, `-`, `*`, `/`, `^`, `(`)
valid_vars = list(beta = 0.1, I = 30)

expr = ~ beta * I / 100
parsed_expr = parser(expr)

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

f$fn()
f$gr()
