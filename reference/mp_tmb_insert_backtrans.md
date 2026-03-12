# Insert Back Transformations of Model Parameters

Insert Back Transformations of Model Parameters

## Usage

``` r
mp_tmb_insert_backtrans(
  model,
  variables = character(),
  transformation = mp_log
)

mp_tmb_implicit_backtrans(model, variables = character())
```

## Arguments

- model:

  TMB model spec object produced using
  [`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
  or
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- variables:

  Character vector of parameters to back transform.

- transformation:

  A transformation object such as
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md),
  which is the default. See the help page for
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  for available options.

## Value

A new model specification object with expressions for the untransformed
(or back transformed) parameters at the beginning of the `before` phase.
The transformed version of the parameter is also added to the defaults
and are identified with a prefixed name (e.g., `log_beta` if `beta` is
log transformed).

## Functions

- `mp_tmb_implicit_backtrans()`: Insert parameter transformations
  implicitly by pre-pending the name of the transformations in front of
  the names of the `variables` that you ask for (e.g.,
  `"log_case_reports"`) even if this variable is not in the model as
  long as the base name (e.g., `"case_reports"`) is.

## See also

[`mp_tmb_insert_trans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_trans.md)

## Examples

``` r
init_si = ("starter_models"
  |> mp_tmb_library("si", package = "macpan2")
  |> mp_tmb_insert_backtrans("beta", mp_log)
  |> mp_initial_list()
)
print(init_si$log_beta)
#>           [,1]
#> [1,] -1.609438
print(log(init_si$beta))
#>           [,1]
#> [1,] -1.609438
```
