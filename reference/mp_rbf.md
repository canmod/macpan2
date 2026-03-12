# Fit a Time-Varying Parameter with Radial Basis Functions

Pass the output of this function to the `tv` argument of
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
to model time variation of a parameter with flexible radial basis
functions.

## Usage

``` r
mp_rbf(
  tv,
  dimension,
  initial_weights,
  seed,
  prior_sd = 1,
  fit_prior_sd = TRUE,
  sparse_tol = 0.01
)
```

## Arguments

- tv:

  String giving the name of the parameter.

- dimension:

  Number of bases.

- initial_weights:

  Optional vector with `dimensions` elements. These are the parameters
  that are fitted and determine how `tv` varies with time.

- seed:

  Optional random seed to use to generate the `initial_weights` if they
  are not provided.

- prior_sd:

  Prior standard deviation default value for radial basis function
  coefficients, defaults to 1.

- fit_prior_sd:

  Should the prior sd be be fitted.

- sparse_tol:

  Tolerance below which radial basis function outputs are set exactly to
  zero. Small values are more accurate but slower. Lack of accuracy can
  be visually apparent as jumps in graphs of the time-varying parameter.
