# Make a Calibrator

Construct an object that can get used to calibrate an object produced by
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
or
[`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md),
and possibly modified by
[`mp_tmb_insert`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
or
[`mp_tmb_update`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md).
Note that the output of this function is not a model that has been
calibrated to data, but rather an object that contains a model that can
be calibrated. To actually attempt a calibration one needs the
[`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)
function (see examples below).

## Usage

``` r
mp_tmb_calibrator(
  spec,
  data = empty_trajectory,
  traj = character(),
  par = character(),
  tv = character(),
  outputs = traj,
  default = list(),
  inits = list(),
  time = NULL,
  save_data = TRUE,
  optimize = FALSE
)
```

## Arguments

- spec:

  An TMB model spec to fit to data. Such specs can be produced by
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
  or
  [`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md),
  and possibly modified with
  [`mp_tmb_insert`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
  and
  [`mp_tmb_update`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md).

- data:

  A data frame containing trajectories to fit to and possibly
  time-varying parameters. The data must be of the same format as that
  produced by
  [`mp_trajectory`](https://canmod.github.io/macpan2/reference/mp_trajectory.md).

- traj:

  A character vector giving the names of trajectories to fit to data, or
  a named list of likelihood distributions specified with a
  [`distribution`](https://canmod.github.io/macpan2/reference/distribution.md)
  for each trajectory. Transformations of trajectories cannot be named
  implicitly unless they are also transformed through the `outputs`
  argument and their transformed version appears in the `data`.

- par:

  A character vector giving the names of parameters (either time-varying
  or not) to fit using trajectory match, or a named list of prior
  distributions specified with a
  [`distribution`](https://canmod.github.io/macpan2/reference/distribution.md)
  for each parameter. Parameters can be implicitly fitted on a
  transformed scale by prefixing the name of the parameter with the name
  of the transformation (e.g., `log_beta` will fit `beta` on the
  log-transformed scale, and
  [`mp_tmb_coef`](https://canmod.github.io/macpan2/reference/mp_tmb_coef.md)
  will report estimates on the original scale by default. The
  [`mp_tmb_implicit_backtrans`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_backtrans.md)
  function is used internally. See that help page for available
  transformations.

- tv:

  A character vector giving the names of parameters to make time-varying
  according to the values in `data`, or a radial basis function
  specified with
  [`mp_rbf`](https://canmod.github.io/macpan2/reference/mp_rbf.md).

- outputs:

  A character vector of outputs that will be generated when
  [`mp_trajectory`](https://canmod.github.io/macpan2/reference/mp_trajectory.md),
  [`mp_trajectory_sd`](https://canmod.github.io/macpan2/reference/mp_trajectory.md),
  or
  [`mp_trajectory_ensemble`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  are called on the optimized calibrator. By default it is just the
  trajectories listed in `traj`. Outputs can be implicitly transformed
  by prefixing the name of the output with the name of the
  transformation (e.g., `log_infection` will output `log(infection)`,
  but
  [`mp_trajectory_sd`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  will report `infection` and its confidence interval on the original
  scale). The
  [`mp_tmb_implicit_trans`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_trans.md)
  function is used internally. See that help page for available
  transformations.

- default:

  A list of default values to use to update the defaults in the `spec`.
  By default nothing is updated. Alternatively one could use
  [`mp_tmb_update`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
  to update the spec outside of the function. Indeed such an approach is
  necessary if new expressions, in addition to default updates, need to
  be added to the spec (e.g. seasonally varying transmission).

- inits:

  An optional list of initial values for the state variables. These
  initial values can be added to the `default` list with identical
  results, but adding them to `inits` is better practice because it
  makes it clear that they are initial values that will change as the
  state updates.

- time:

  Specify the start and end time of the simulated trajectories, and the
  time period associated with each time step. The default is `NULL`,
  which takes simulation bounds from the `data`. You can use
  [`mp_sim_bounds`](https://canmod.github.io/macpan2/reference/mp_sim_bounds.md)
  and
  [`mp_sim_offset`](https://canmod.github.io/macpan2/reference/mp_sim_offset.md)
  to be more specific. See the example on the
  [`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)
  help file for an illustration the use of
  [`mp_sim_offset`](https://canmod.github.io/macpan2/reference/mp_sim_offset.md).

- save_data:

  Save a copy of the data in the calibrator object that is returned, so
  that you do not need to pass the data manually to downstream functions
  like
  [`mp_forecaster`](https://canmod.github.io/macpan2/reference/mp_forecaster.md).
  It the resulting calibrator object is so large that it causes you
  problems, consider not saving the data in the calibrator object and
  manually passing it to the data argument of
  [`mp_forecaster`](https://canmod.github.io/macpan2/reference/mp_forecaster.md).

- optimize:

  Call
  [`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)
  on the resulting calibrator object, before returning it. The default
  is `FALSE` so that you can control when you would like to spend time
  optimizing.

## Examples

``` r
spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
sim = mp_simulator(spec, 50, "infection")
data = mp_trajectory(sim)
cal = mp_tmb_calibrator(
    spec
  , data
  , traj = "infection"
  , par = "beta"
  , default = list(beta = 0.25)
)
mp_optimize(cal)
#> $par
#> params 
#>    0.2 
#> 
#> $objective
#> [1] 49.74796
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 5
#> 
#> $evaluations
#> function gradient 
#>        6        6 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
if (suppressPackageStartupMessages(require(broom.mixed))) {
  print(mp_tmb_coef(cal))
}
#>     term  mat row col default  type estimate   std.error
#> 1 params beta   0   0    0.25 fixed      0.2 0.009166433
```
