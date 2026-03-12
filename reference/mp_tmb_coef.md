# TMB Model Coefficient Table

TMB Model Coefficient Table

## Usage

``` r
mp_tmb_coef(model, back_transform = TRUE, ...)
```

## Arguments

- model:

  Object that contains information about model coefficients.

- back_transform:

  A boolean to indicate if model coefficients should be back transformed
  to display their defaults, estimates, and confidence intervals on the
  original scale. Coefficient names are also stripped of their
  transformation identifier. Currently, this back transformation only
  applies to log transformed coefficients that have been named with
  "log\_" prefix or logit transformed coefficients that have been named
  with "logit\_" prefix. Back transformation also applies to time
  varying parameters and distributional parameters that get automatic
  prefixes when used. `back_transform` defaults to `TRUE`.

- ...:

  Arguments to pass onto the
  [`broom.mixed::tidy.TMB`](https://rdrr.io/pkg/broom.mixed/man/tidy.TMB.html)
  method. To get confidence intervals, use `conf.int = TRUE`. Note that
  there is currently an issue when using `effects = "random`.

## Value

A data frame that describes the fitted coefficients.
