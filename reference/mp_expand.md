# Expand Model

Expand a structured model so that it is represented in an unstructured
format requiring a more verbose description. Currently, this is only
applicable for
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
objects that have explicit flows (e.g.
[`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)).
For such models, `mp_expand` produces a model with expression lists
composed entirely of plain R formulas.

## Usage

``` r
mp_expand(model)

mp_reduce(model)
```

## Arguments

- model:

  A model object.

## Functions

- `mp_reduce()`: Confusingly, `mp_reduce` and `mp_expand` are synonyms.
  Please use `mp_expand` in new projects, as `mp_reduce` is available
  for back-compatibility only.

## Examples

``` r
sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
print(sir)
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I / N", 
#>      flow_name = "infection")
#> 2: mp_per_capita_flow(from = "I", to = "R", rate = "gamma", flow_name = "recovery")
#> 
print(mp_expand(sir))
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: infection ~ S * (beta * I/N)
#> 2: recovery ~ I * (gamma)
#> 3: S ~ S - infection
#> 4: I ~ I + infection - recovery
#> 5: R ~ R + recovery
#> 
```
