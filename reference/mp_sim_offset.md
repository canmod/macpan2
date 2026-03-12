# Simulation Offsets

Offset the starting and ending times of the simulation, from the start
and end time of the data used in calibration. This is used to override
the default offsets of zero taken from the observed data passed to
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Usage

``` r
mp_sim_offset(
  sim_start_offset,
  sim_end_offset,
  time_scale = "steps",
  time_column = "time"
)
```

## Arguments

- sim_start_offset:

  Number of time steps before the first data point to start each
  simulation.

- sim_end_offset:

  Number of time steps after the last data point to end each simulation.

- time_scale:

  Qualitative description of the size of a time step. The only valid
  values are 'steps' and 'daily'. If you would like each time step in
  the model to represent one day, then you should consider using
  'daily'. Note that using 'daily' will require that your data represent
  time using a (1) [`Date`](https://rdrr.io/r/base/Dates.html)
  vector, (2) [`character`](https://rdrr.io/r/base/character.html)
  vector in YYYY-MM-DD format, or (3)
  [`integer`](https://rdrr.io/r/base/integer.html) vector that counts
  the number of days since some reference. Otherwise please choose
  'steps', the default, and convert your time column into integer values
  that represent the time step that you would like in the model.

- time_column:

  Name of the column that will identify the time at which particular
  values were observed.

## Value

An object to be passed to the `time` argument of
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## See also

[`mp_sim_bounds()`](https://canmod.github.io/macpan2/reference/mp_sim_bounds.md)
