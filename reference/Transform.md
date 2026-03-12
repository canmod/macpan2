# Transform

Transform

## Usage

``` r
Transform(variable, default = NULL, trans_variable = variable)

Identity(variable, default = NULL, trans_variable = variable)

Log(variable, default = NULL, trans_variable = sprintf("log_%s", variable))

Logit(
  variable,
  default = NULL,
  trans_variable = sprintf("logit_%s", variable)
)
```

## Arguments

- variable:

  Character string giving the name of a variable in the model.

- default:

  Default value for the untransformed variable. If `NULL` (the default)
  this value is taken from the initial value in the model containing the
  transformation.

- trans_variable:

  Character string to use as the name of the transformed version of the
  `variable`.

## Functions

- `Identity()`: Identity transformation.

- `Log()`: Log transformation.

- `Logit()`: Logit transformation.
