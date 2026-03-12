# Simulate Dynamical Model Trajectories

Return simulations of the trajectory of the output variables of a
dynamical model simulator. To see this functionality in context, please
see
[`vignette("quickstart")`](https://canmod.github.io/macpan2/articles/quickstart.md).

## Usage

``` r
mp_trajectory(model, include_initial = FALSE)

mp_trajectory_par(
  model,
  parameter_updates = list(),
  include_initial = FALSE,
  include_final = FALSE,
  baseline = c("recommended", "default", "optimized")
)

mp_trajectory_sd(
  model,
  conf.int = FALSE,
  conf.level = 0.95,
  include_initial = FALSE,
  back_transform = TRUE
)

mp_trajectory_ensemble(model, n, probs = c(0.025, 0.975))

mp_trajectory_sim(model, n, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

mp_trajectory_replicate(
  model,
  n,
  parameter_updates = list(),
  include_initial = FALSE,
  include_final = FALSE,
  baseline = c("recommended", "default", "optimized")
)
```

## Arguments

- model:

  A dynamical model simulator produced by
  [`mp_simulator`](https://canmod.github.io/macpan2/reference/mp_simulator.md).

- include_initial:

  Should the initial values of the simulation be included in the output?
  If `TRUE` this will include outputs for `time == 0` associated with
  the initial values. See
  [`mp_initial`](https://canmod.github.io/macpan2/reference/mp_initial.md)
  for another approach to getting the initial values.

- parameter_updates:

  Named list of a subset of model variables with the values to use when
  simulating the trajectory using the `mp_trajectory_par` function. In
  the future we plan to allow this variable to be a data frame with one
  row for each scalar value (which would be useful if only certain
  elements of a vector or matrix are parameters) and a string giving the
  name of a file containing parameter information. But for now, only a
  list is allowed.

- include_final:

  Should the final values of the simulation, after the post-simulation
  processing steps in the `after` stage of a model, be included in the
  output? If `TRUE` this will include outputs for
  `time == time_steps + 1`, associated with the values of the variables
  after the full trajectory has been post-processed in the `after`
  stage. See
  [`mp_final`](https://canmod.github.io/macpan2/reference/mp_final.md)
  for another approach to getting the final values.

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

- conf.int:

  Should confidence intervals be produced?

- conf.level:

  If `conf.int` is `TRUE`, what confidence level should be used? For
  example, the default of `0.95` corresponds to 95% confidence
  intervals.

- back_transform:

  A boolean to indicate if trajectories, standard deviations, and
  confidence intervals should be back transformed to the original scale.
  Variable names are also stripped of their transformation identifier.
  Currently, this back transformation only applies to log transformed
  coefficients that have been named with "log\_" prefix or logit
  transformed coefficients that have been named with "logit\_" prefix.
  Back transformation also applies to time varying parameters and
  distributional parameters that get automatic prefixes when used.
  `back_transform` defaults to `TRUE`.

- n:

  Number of random trajectories to simulate.

- probs:

  Numeric vector of probabilities corresponding to quantiles for
  summarizing the results over the random realizations.

## Value

A data frame with one row for each simulated value and the following
columns.

- matrix:

  Name of the variable in the model. All variables are matrix-valued in
  `macpan2` (scalars are technically 1-by-1 matrices), which explains
  the name of this field. In hindsight I would have called it
  `variable`.

- time:

  Time index of the simulated value, with `time = 0` indicating initial
  values.

- row:

  The 0-based index of the row of the matrix, or the name of the row of
  the matrix if row names (or names for column vectors) are supplied for
  the default value of the matrix.

- col:

  The 0-based index of the column of the matrix, or the name of the
  column of the matrix if column names are supplied for the default
  value of the matrix. It is also possible that this column is blank if
  everything is either a scalar or column vector (a common case).

- value:

  (`mp_trajectory` and `mp_trajectory_sd`) Simulation values.

- sd:

  (for `mp_trajectory_sd` only) The standard deviations of the simulated
  values accounting for parameter estimation uncertainty.

- conf.low:

  (for `mp_trajectory_sd` only) The lower bounds of the confidence
  interval for the simulated values.

- conf.high:

  (for `mp_trajectory_sd` only) The upper bounds of the confidence
  interval for the simulated values.

- n%:

  (for `mp_trajectory_[ensemble|sim]`) The n-th quantiles of the
  simulation values over repeated simulations.

## Functions

- `mp_trajectory_par()`: Produce a trajectory, after updating the
  `baseline` set of parameters with values in `parameter_updates`.

- `mp_trajectory_sd()`: Simulate a trajectory that includes uncertainty
  information provided by the `sdreport` function in `TMB` with default
  settings.

- `mp_trajectory_ensemble()`: Simulate a trajectory that includes
  uncertainty information provided by repeatedly sampling from a normal
  approximation to the distribution of the fitted parameters, and
  generating one trajectory for each of these samples. The quantiles of
  the empirical distribution of these trajectories can be used to
  produce a confidence interval for the fitted trajectory.

- `mp_trajectory_sim()`: Generate quantiles over `n` realizations of the
  trajectory. Instead of a `value` column in the output data frame,
  there is one column for each of the quantiles defined in `probs`.

- `mp_trajectory_replicate()`: Generate a list of `n` simulation
  results.

## Examples

``` r
spec = mp_tmb_library("starter_models"
  , "si"
  , package = "macpan2"
)
simulator = mp_simulator(spec
  , time_steps = 10L
  , outputs = c("infection", "I")
)
trajectory = mp_trajectory(simulator)
print(trajectory)
#>       matrix time row col     value
#> 1          I    1   0   0 1.1980000
#> 2  infection    1   0   0 0.1980000
#> 3          I    2   0   0 1.4347296
#> 4  infection    2   0   0 0.2367296
#> 5          I    3   0   0 1.7175586
#> 6  infection    3   0   0 0.2828290
#> 7          I    4   0   0 2.0551703
#> 8  infection    4   0   0 0.3376117
#> 9          I    5   0   0 2.4577569
#> 10 infection    5   0   0 0.4025866
#> 11         I    6   0   0 2.9372272
#> 12 infection    6   0   0 0.4794702
#> 13         I    7   0   0 3.5074180
#> 14 infection    7   0   0 0.5701908
#> 15         I    8   0   0 4.1842977
#> 16 infection    8   0   0 0.6768796
#> 17         I    9   0   0 4.9861405
#> 18 infection    9   0   0 0.8018428
#> 19         I   10   0   0 5.9336454
#> 20 infection   10   0   0 0.9475049
```
