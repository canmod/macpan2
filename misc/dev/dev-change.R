library(macpan2)

sims = simple_sims(
  iteration_exprs = list(
    j ~ change(j, change_points),
    y ~ x[j]
  ),
  time_steps = 10,
  j = 0,
  change_points = c(0, 4, 7),
  x = c(42, pi, sqrt(2)),
  y = empty_matrix
)
sims
