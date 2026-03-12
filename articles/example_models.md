# Example Models

[![status](https://img.shields.io/badge/status-mature%20draft-yellow)](https://canmod.github.io/macpan2/articles/vignette-status#mature-draft)

## Finding Example Models

The `macpan2` comes with a set of example model definitions, which can
be listed with the
[`mp_show_models()`](https://canmod.github.io/macpan2/reference/mp_show_models.md)
function.

``` r
mp_show_models()
```

| Directory                                                                                                                    | Title                           | Description                                                                                                     |
|:-----------------------------------------------------------------------------------------------------------------------------|:--------------------------------|:----------------------------------------------------------------------------------------------------------------|
| [awareness](https://github.com/canmod/macpan2/tree/main/inst/starter_models/awareness)                                       | Awareness Models                | Behaviour modifications in response to death                                                                    |
| [fibonacci](https://github.com/canmod/macpan2/tree/main/inst/starter_models/fibonacci)                                       | Fibonacci Numbers               | Matrix population model for generating the Fibonacci numbers                                                    |
| [hiv](https://github.com/canmod/macpan2/tree/main/inst/starter_models/hiv)                                                   | HIV                             | A simple HIV model                                                                                              |
| [lotka_volterra_competition](https://github.com/canmod/macpan2/tree/main/inst/starter_models/lotka_volterra_competition)     | Lotka-Volterra                  | Simple two-species competition model                                                                            |
| [lotka_volterra_predator_prey](https://github.com/canmod/macpan2/tree/main/inst/starter_models/lotka_volterra_predator_prey) | Lotka-Volterra                  | Simple predator-prey model                                                                                      |
| [macpan_base](https://github.com/canmod/macpan2/tree/main/inst/starter_models/macpan_base)                                   | Macpan Base                     | Re-implementation of the McMaster group’s COVID-19 model                                                        |
| [nfds](https://github.com/canmod/macpan2/tree/main/inst/starter_models/nfds)                                                 | NFDS and Vaccine Design         | An ecological model using population genomics to design optimal vaccines as implemented in Colijn et al. (2020) |
| [seir](https://github.com/canmod/macpan2/tree/main/inst/starter_models/seir)                                                 | Basic SEIR                      | Simple epidemic model with an exposed class                                                                     |
| [shiver](https://github.com/canmod/macpan2/tree/main/inst/starter_models/shiver)                                             | SHIVER = SEIR + H + V           | A modified SEIR model with Hospitalization and Vaccination                                                      |
| [si](https://github.com/canmod/macpan2/tree/main/inst/starter_models/si)                                                     | Basic SI                        | A very simple epidemic model                                                                                    |
| [si_behaviour](https://github.com/canmod/macpan2/tree/main/inst/starter_models/si_behaviour)                                 | SI with Behaviour               | SI model with behavioural protection in response to the number of infectious individuals                        |
| [sir](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir)                                                   | Basic SIR                       | A very simple epidemic model                                                                                    |
| [sir_age](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir_age)                                           | Age-stratified SIR              | An age-stratified SIR model                                                                                     |
| [sir_demog](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir_demog)                                       | SIR with Demography             | An SIR model with birth and death                                                                               |
| [sir_mosquito](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir_mosquito)                                 | Mosquito-Vector SIR             | SIR model for mosquito vectors                                                                                  |
| [sir_waning](https://github.com/canmod/macpan2/tree/main/inst/starter_models/sir_waning)                                     | SIR with Waning Immunity (SIRS) | A basic SIR model with a flow from R back to S                                                                  |
| [ww](https://github.com/canmod/macpan2/tree/main/inst/starter_models/ww)                                                     | Wastewater Model                | Macpan base with an additional wastewater component                                                             |

There are three things that you can do with the items on this list: read
them, use them, and modify them. To read them, just click on the links
above to take you to each model. To use them and modify them, please
continue reading.

## Using Examples

To use the `sir` example it can be read into R using the following code.

``` r
sir_dir = system.file("starter_models", "sir", package = "macpan2")
sir = mp_tmb_library(sir_dir)
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
```

To see how to actually generate simulations from this model see [this
article](https://canmod.github.io/macpan2/articles/quickstart). To use
another model, again, replace `sir` with another entry in the `dir`
column above.

## Modifying Examples

To take `sir` as a jumping-off point for producing your own model one
may use the following code.

``` r
my_sir_dir = file.path(tempdir(), "my_sir")
mp_model_starter("sir", my_sir_dir)
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
```

After running this code you can go to the files in `my_sir_dir` and
modify what you see there. Note that you typically want to chose a
specific directory for your model instead of using `tempdir`. You still
need to read your own model in the usual way.

``` r
my_sir = mp_tmb_library(my_sir_dir)
print(my_sir)
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
```

These look identical to what came before, but that’s just because it
hasn’t been modified … yet …
