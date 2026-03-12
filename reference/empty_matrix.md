# Empty Matrix

Empty matrices are useful when defining matrices that do not need to be
initialized because they will get computed before they are required by
other expressions. They can also provide a useful placeholder for
matrices that should only have a value after a certain phase in the
simulation.

## Usage

``` r
empty_matrix
```

## Format

A [`numeric`](https://rdrr.io/r/base/numeric.html)
[`matrix`](https://rdrr.io/r/base/matrix.html) with zero rows and zero
columns.

## Examples

``` r
spec = mp_tmb_model_spec(during = list(x ~ time_step(0)))
identical(spec$empty_matrices()$x, empty_matrix) ## TRUE
#> [1] TRUE
```
