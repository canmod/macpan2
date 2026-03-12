# Comparison Functions

Comparison Functions

## Usage

``` r
all_equal(x, y)

all_consistent(x, y)

not_all_equal(x, y)

all_not_equal(x, y)
```

## Arguments

- x:

  [`character`](https://rdrr.io/r/base/character.html) object

- y:

  [`character`](https://rdrr.io/r/base/character.html) object

## Functions

- `all_equal()`: Is it true that all corresponding elements of `x` and
  `y` are equal, have the same shape, and have no missing values?

- `all_consistent()`: Is it true that all corresponding elements of `x`
  and `y` are either equal or at least one is a blank string, have the
  same shape, and have no missing values?

- `not_all_equal()`: Complement of `all_equal`.

- `all_not_equal()`: Do not know yet. Currently unused; should we
  remove?
