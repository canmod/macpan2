# Insert Basic Transformations of Model Variables

Insert Basic Transformations of Model Variables

## Usage

``` r
mp_tmb_insert_trans(model, variables = character(), transformation = mp_log)

mp_tmb_implicit_trans(model, variables = character())
```

## Arguments

- model:

  TMB model spec object produced using
  [`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
  or
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- variables:

  Character vector of variables to transform.

- transformation:

  A transformation object such as
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md),
  which is the default. See the help page for
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  for available options.

## Value

A new model specification object with expressions for the transformed
variables at the end of the simulation loop. The transformed variables
are identified with a prefixed name (e.g., `log_incidence` if
`incidence` is log transformed).

## Functions

- `mp_tmb_implicit_trans()`: Insert variable transformations implicitly
  by pre-pending the name of the transformations in front of the names
  of the `variables` that you ask for (e.g., `"log_case_reports"`) even
  if this variable is not in the model as long as the base name (e.g.,
  `"case_reports"`) is.

## See also

[`mp_tmb_insert_backtrans()`](https://canmod.github.io/macpan2/reference/mp_tmb_insert_backtrans.md)

## Examples

``` r
("starter_models"
  |> mp_tmb_library("si", package = "macpan2")
  |> mp_tmb_insert_trans("infection", mp_log)
  |> mp_simulator(time_steps = 5L, outputs = "log_infection")
  |> mp_trajectory()
)
#>          matrix time row col     value
#> 1 log_infection    1   0   0 -1.619488
#> 2 log_infection    2   0   0 -1.440837
#> 3 log_infection    3   0   0 -1.262913
#> 4 log_infection    4   0   0 -1.085859
#> 5 log_infection    5   0   0 -0.909845
```
