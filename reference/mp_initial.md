# Initial Values of Variables Immediately Before the Simulation Loop

Return a data frame containing the values of variables at the end of the
`before` phase, right before the simulation loop begins (i.e. right
before the `during` phase).

## Usage

``` r
mp_initial(model)

mp_initial_list(model)
```

## Arguments

- model:

  A model specification object or model simulator object from which to
  extract initial values.

## Value

A long-format data frame with initial values for matrices. The columns
of this output are `matrix`, `time`, `row`, `col`, and `value`. Scalar
matrices do not have any entries in the `row` or `col` columns. The
`before` phase corresponds to a `time` value of 0.

## Functions

- `mp_initial_list()`: List of the initial variables as matrices.
