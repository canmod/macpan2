# Data Frame Describing Each Change to Each State Variable

Get a data frame with one row for each change made to each state
variable at each time step.

## Usage

``` r
mp_change_frame(spec)
```

## Arguments

- spec:

  Model specification
  ([`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)).

## Value

Data frame with two columns: `state` and `change`. Each row describes
one change.

## Examples

``` r
("starter_models"
  |> mp_tmb_library("sir", package = "macpan2") 
  |> mp_change_frame()
)
#>   state     change
#> 1     S -infection
#> 2     I +infection
#> 3     I  -recovery
#> 4     R  +recovery
```
