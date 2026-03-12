# Read Item from a Model Library

Get a TMB model specification from a model library.

## Usage

``` r
mp_tmb_library(..., package = NULL, alternative_specs = FALSE)

mp_tmb_entire_library()

mp_official_library(model_name)
```

## Arguments

- ...:

  File path components pointing to a directory that contains an R script
  that creates an object called `spec`, which is produced by
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- package:

  If `NULL`, [`file.path`](https://rdrr.io/r/base/file.path.html) is
  used to put together the `...` components but if `package` is the name
  of a package (as a character string) then
  [`system.file`](https://rdrr.io/r/base/system.file.html) is used to
  put together the `...` components.

- alternative_specs:

  If `TRUE`, return a list of alternative specification objects. For
  models without alternatives this will cause the return value to be a
  list with one element containing a spec object.

- model_name:

  Character string giving the name of a single model in the official
  `starter_models`
  [library](https://github.com/canmod/macpan2/tree/main/inst/starter_models).

## Details

This function executes the `R` code in the `tmb.R` file of a directory
in a model library. To be a valid model, this file should produce an
object called `spec` (containing a model specification produced by the
[`mp_tmb_model_spec()`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
function) or `specs` (containing a list of such specifications). This
`mp_tmb_library()` function returns this `spec` or `specs` object (see
the `alternative_specs` argument), but does not expose any other object
produced by `tmb.R`.

## Functions

- `mp_tmb_entire_library()`: List of model specifications; one for each
  model in the library.

- `mp_official_library()`: Get a model specification from the official
  `macpan2` `starter_models`
  [library](https://github.com/canmod/macpan2/tree/main/inst/starter_models).

## See also

[`mp_show_models()`](https://canmod.github.io/macpan2/reference/mp_show_models.md)

## Examples

``` r
mp_tmb_library(
    "starter_models"
  , "si"
  , package = "macpan2"
)
#> ---------------------
#> Default values:
#>  quantity value
#>         N 100.0
#>      beta   0.2
#>         I   1.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - 1
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I / N", 
#>      flow_name = "infection")
#> 
```
