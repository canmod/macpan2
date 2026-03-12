# Kronecker Operator

Convert a scalar binary operator into one that performs a Kronecker
product with more convenient row and column names for use with
`macpan2`. The result is numerically identical to base `R`'s `%x%` and
[`kronecker()`](https://rdrr.io/r/base/kronecker.html), but with cleaner
naming of rows and columns.

## Usage

``` r
mp_kronecker_operator(operator)
```

## Arguments

- operator:

  A scalar binary operator.

## Value

A Kronecker operator convenient for use with `macpan2`.
