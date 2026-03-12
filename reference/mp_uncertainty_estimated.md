# Uncertainty Estimated

Does a model contain estimates of parameter uncertainty, probably
through a covariance matrix estimated using
[`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)?

## Usage

``` r
mp_uncertainty_estimated(model)
```

## Arguments

- model:

  A model that can be calibrated, probably produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Value

Either `TRUE` or `FALSE`.
