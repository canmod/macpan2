# Make a Forecaster

A forecaster is an object that can make forecasts. It is constructed
from
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md),
which is an object that can be used to calibrate the parameters of a
model – typically by fitting the model to data. The main difference
between a calibrator and a forecaster is that the latter runs for more
time steps, past the last time step of the calibrator. A forecaster
object can be used to generate different types of forecasts using the
[`mp_trajectory`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
family of functions.

## Usage

``` r
mp_forecaster(
  calibrator,
  forecast_period_time_steps,
  outputs = NULL,
  data = NULL,
  tv = NULL,
  default = list(),
  inits = list()
)
```

## Arguments

- calibrator:

  Object made using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md),
  which can be calibrated to data.

- forecast_period_time_steps:

  The number of time steps to project beyond the period with data.

- outputs:

  An optional character vector of variables in the model to forecast. If
  this argument is omited, then the forecaster will inherit the outputs
  of the `calibrator` object. Note that if you prefix the name of an
  output using `log_`, `logit_`, or `sqrt_` then the transformed version
  of the outputs will be simulated. This technique is useful for
  confidence intervals produced by
  [`mp_trajectory_sd`](https://canmod.github.io/macpan2/reference/mp_trajectory.md)
  that go out of the valid range of values (e.g., use `log_` for
  confidence intervals of negative state variables).

- data:

  An optional data frame containing the data that were fitted to.
  Typically this argument will be unused, because by default the data
  are stored in the `calibrator` unless you want to avoid making too
  many copies of the data.

- tv:

  An optional replacement for the `tv` parameter in the
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
  function.

- default:

  An optional list of default model variables (e.g., parameters initial
  values of state variables) to override calibrated values.

- inits:

  An optional list of initial values for the state variables. These
  initial values can be added to the `default` list with identical
  results, but adding them to `inits` is better practice because it
  makes it clear that they are initial values that will change as the
  state updates.
