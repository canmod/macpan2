# Self Cartesian Product Excluding One Off-Diagonal Side

Self Cartesian Product Excluding One Off-Diagonal Side

## Usage

``` r
mp_triangle(
  x,
  y_labelling_column_names,
  exclude_diag = TRUE,
  lower_tri = FALSE
)
```

## Arguments

- x:

  An index.

- y_labelling_column_names:

  TODO

- exclude_diag:

  Should 'diagonal' commponents be excluded from the output.

- lower_tri:

  Should the lower triangular components be include from the output. If
  `FALSE` the result is upper triangular.

## See also

Other functions that take products of index tables and return one index
tables
[`mp_cartesian()`](https://canmod.github.io/macpan2/reference/mp_cartesian.md),
[`mp_linear()`](https://canmod.github.io/macpan2/reference/mp_linear.md),
[`mp_square()`](https://canmod.github.io/macpan2/reference/mp_square.md),
[`mp_symmetric()`](https://canmod.github.io/macpan2/reference/mp_symmetric.md)
