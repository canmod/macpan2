# Model Coefficient Table with stan

Leverages the `tmbstan` and `broom.mixed` packages to generate
MCMC-based coefficient tables.

## Usage

``` r
mp_tmbstan_coef(model, tmbstan_args = list(), ...)
```

## Arguments

- model:

  Object that contains information about model coefficients.

- tmbstan_args:

  Arguments to pass on to `tmbstan`, which is used to generate an
  `rstan` object from the underlying `TMB` object.

- ...:

  Arguments to pass onto the
  [`broom.mixed::tidy.stanfit`](https://rdrr.io/pkg/broom.mixed/man/mcmc_tidiers.html)
  method.
