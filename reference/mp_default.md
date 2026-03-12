# Default Values

Default Values

## Usage

``` r
mp_default(model, include_all = FALSE)

mp_default_list(model, include_all = FALSE)
```

## Arguments

- model:

  A model object from which to extract default values. If `model` is a
  calibrator object (see
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md))
  that has been optimized (using
  [`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md)),
  then the values returned by `mp_default` and `mp_default_list` are
  updated to reflect this calibration/optimization process.

- include_all:

  Include all default variables, even those that are not used in the
  `before`, `during`, or `after` phase of the simulations. When
  `include_all` is `FALSE`, examples of excluded variables are those
  used by an objective function only or those intended to be used in an
  extended model specification produced using functions like
  [`mp_tmb_insert`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
  and
  [`mp_tmb_update`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md).

## Value

A long-format data frame with default values for matrices required as
input to model objects. The columns of this output are `matrix`, `row`,
`col`, and `value`. Scalar matrices do not have any entries in the `row`
or `col` columns.

## Functions

- `mp_default_list()`: List of the default variables as matrices.
