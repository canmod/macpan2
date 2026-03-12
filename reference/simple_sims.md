# Simple Iterated Simulation

Simple Iterated Simulation

## Usage

``` r
simple_sims(iteration_exprs, time_steps, int_vecs = list(), mats = list())
```

## Arguments

- iteration_exprs:

  List of expressions to pass to the
  [engine](https://canmod.github.io/macpan2/articles/cpp_side). The
  expressions are only allowed to use valid
  [`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md).
  Each expression is evaluated in order, once for each iteration. The
  number of iterations is controlled by the `time_steps` argument.

- time_steps:

  Number of time steps to iterate.

- int_vecs:

  Named list of integer vectors.

- mats:

  Named list of matrices.

## Value

A data frame with the simulation results.
