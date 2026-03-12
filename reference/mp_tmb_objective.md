# Value of the Objective Function of a Model

Value of the Objective Function of a Model

## Usage

``` r
mp_tmb_objective(
  model,
  parameter_updates = list(),
  baseline = c("recommended", "default", "optimized")
)
```

## Arguments

- model:

  A model with an objective function, probably one produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

- parameter_updates:

  Named list of a subset of model variables with the values to use when
  simulating the trajectory using the `mp_trajectory_par` function. In
  the future we plan to allow this variable to be a data frame with one
  row for each scalar value (which would be useful if only certain
  elements of a vector or matrix are parameters) and a string giving the
  name of a file containing parameter information. But for now, only a
  list is allowed.

- baseline:

  Models can contain several alternative sets of parameters, and this
  `baseline` argument is used to choose which of these should be updated
  using the `parameter_updates` passed to `mp_trajectory_par`. The
  current options are `"recommended"`, `"optimized"`, and `"default"`.
  The `"recommended"` option will be used if neither of the other two
  options are selected. If `model` is capable of being optimized (e.g.,
  it was created using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md))
  then `"recommended"` is equivalent to `"optimized"`, which use the
  best set of parameters found by
  [`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md).
  If
  [`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)
  has not yet been called on `model` then a warning will be issued. If
  `model` is not capable of being optimized then `"recommended"` is
  equivalent to `"default"`, which uses the original set of parameters
  available when `model` was created.
