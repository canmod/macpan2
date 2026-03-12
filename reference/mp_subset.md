# Subset of Indexes

Take a subset of the rows of an index table (see
[`mp_index`](https://canmod.github.io/macpan2/reference/mp_index.md)) to
produce another index table. The `mp_subset` function gives rows that
match a certain criterion and `mp_setdiff` gives rows that do not match.

## Usage

``` r
mp_subset(x, ...)

mp_setdiff(x, ...)
```

## Arguments

- x:

  Model index.

- ...:

  Name-value pairs. The names are columns (or sets of columns using
  dot-concatenation) in `x` and the values are character vectors that
  refer to labels with respect to those columns. These values determine
  the resulting subset.

## See also

Other functions that return index tables
[`mp_cartesian()`](https://canmod.github.io/macpan2/reference/mp_cartesian.md),
[`mp_index()`](https://canmod.github.io/macpan2/reference/mp_index.md),
[`mp_rename()`](https://canmod.github.io/macpan2/reference/mp_rename.md),
[`mp_union()`](https://canmod.github.io/macpan2/reference/mp_union.md)
