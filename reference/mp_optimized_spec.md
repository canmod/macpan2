# Optimized Model Specification

Create a new model specification using parameter values that have been
optimized.

## Usage

``` r
mp_optimized_spec(model, spec_structure = c("original", "modified"))
```

## Arguments

- model:

  A model object that can be optimized/calibrated. Currently, only
  models produced by
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
  are valid.

- spec_structure:

  A character string identifying the structure of the returned model
  specification. Use `"original"` for the specification originally
  passed to the
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
  function, and `"modified"` for the one that was used to fit to data.
  See the details below for more information on these modifications.

## Value

A copy of the model specification used to produce the calibrator model,
with calibrated default values.

## Details

The following is a list of additional model specification structure that
was or could have been added to the `"modified"` specification by
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
for the purposes of fitting to data.

- Observed data in a format that can be compared with simulated data.

- Expressions preparing simulated data so that they can be compared with
  the observed data.

- Distributional parameters for likelihood and prior components of the
  objective function (e.g., a dispersion parameter for a negatively
  binomial likelihood component, or a prior standard deviation for a
  transmission rate parameter).

- Data, parameters, and expressions for modelling time-variation of
  parameters (e.g., basis functions and weights for a spline determining
  the time-variation of transmission rate).
