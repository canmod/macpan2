library(macpan2)
library(TMB)
library(dplyr)

valid_vars = model_vars(
  states = scalars(S = 99, I = 1, R = 0),
  fixed_parameters = scalars(
    beta = 0.06,
    gamma = 0.15
  ),
  matrix_constants = matrices(
    X = matrix(rnorm(6), 3, 2),
    Y = matrix(rnorm(6), 2, 3)
  ),
  vector_constants = vectors(a = rnorm(3), b = rnorm(6))
)
valid_funcs = nlist(`+`, `*`, `/`, `%*%`, `(`, `sum`, `-`)
valid_vars
ilist(valid_vars)
ff =~ ((X %*% Y) + (S * gamma)) %*% a - 10 * sum(b)
eval_expr(ff, unclassify(valid_vars), valid_funcs)
