# Optimization Attempted

Has an attempt been made to calibrate model parameters through
optimization of a likelihood function or posterior density, probably by
using
[`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)?

## Usage

``` r
mp_opt_attempted(model)
```

## Arguments

- model:

  A model that can be calibrated, probably produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Value

Either `TRUE` or `FALSE`.
