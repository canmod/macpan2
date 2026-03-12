# Find all Paths Through Compartments

Find all paths through a compartmental model.

## Usage

``` r
find_all_paths(edges_df, start_node_guesses = character(0L))
```

## Arguments

- edges_df:

  A data frame with a `from` and a `to` column, which can be extracted
  from a model specification object using the
  [`mp_flow_frame`](https://canmod.github.io/macpan2/reference/mp_flow_frame.md).

- start_node_guesses:

  Optional guesses for nodes that start paths. This is useful for models
  that are not directed acyclic graphs (DAGs).

## Value

List of character vectors of state variable names, each vector
describing a path through the model.

## Examples

``` r
spec = mp_tmb_library("starter_models", "macpan_base", package = "macpan2")
spec |> mp_flow_frame() |> find_all_paths()
#> [[1]]
#> [1] "S"  "E"  "Ia" "R" 
#> 
#> [[2]]
#> [1] "S"  "E"  "Ip" "Im" "R" 
#> 
#> [[3]]
#> [1] "S"  "E"  "Ip" "Is" "D" 
#> 
#> [[4]]
#> [1] "S"  "E"  "Ip" "Is" "H"  "R" 
#> 
#> [[5]]
#> [1] "S"    "E"    "Ip"   "Is"   "ICUd" "D"   
#> 
#> [[6]]
#> [1] "S"    "E"    "Ip"   "Is"   "ICUs" "H2"   "R"   
#> 
```
