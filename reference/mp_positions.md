# Position Vectors

Return an integer vector of positions of `x` in `table`. Currently this
is a simple wrapper around [`match`](https://rdrr.io/r/base/match.html).

## Usage

``` r
mp_positions(x, table, zero_based = TRUE)
```

## Arguments

- x:

  Character vector

- table:

  Character vector

- zero_based:

  Use zero-based indexing? Defaults to `TRUE`, otherwise standard R
  one-based indexing is used.
