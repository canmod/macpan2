# Print a Table of Existing Models

Collects information from the headers of the README files in model
directories and returns the results as a data frame

## Usage

``` r
mp_show_models(
  dir = system.file("starter_models", package = "macpan2"),
  show_missing = FALSE,
  for_markdown = FALSE
)

show_models(
  dir = system.file("starter_models", package = "macpan2"),
  show_missing = FALSE,
  for_markdown = FALSE
)

mp_list_models(dir = system.file("starter_models", package = "macpan2"))
```

## Arguments

- dir:

  directory to list

- show_missing:

  (logical) include entries for models with no README information?

- for_markdown:

  (logical) format for rendering the table with markdown-formatted links
  to model readme files?

## Value

a data frame containing entries `Directory` (model directory), `Title`
(model title), `Description` (short description)

## Functions

- `show_models()`: Synonym for `mp_show_models`, which is preferred.
  Present for back-compatibility.

- `mp_list_models()`: Return a character vector containing model names,
  instead of a data frame with more information about each model.

## Examples

``` r
mp_show_models(show_missing = TRUE)
#>                       Directory                           Title
#> 1                     awareness                Awareness Models
#> 2                     fibonacci               Fibonacci Numbers
#> 3                           hiv                             HIV
#> 4    lotka_volterra_competition                  Lotka-Volterra
#> 5  lotka_volterra_predator_prey                  Lotka-Volterra
#> 6                   macpan_base                     Macpan Base
#> 7                          nfds         NFDS and Vaccine Design
#> 8                          seir                      Basic SEIR
#> 9                        shiver           SHIVER = SEIR + H + V
#> 10                           si                        Basic SI
#> 11                 si_behaviour               SI with Behaviour
#> 12                          sir                       Basic SIR
#> 13                      sir_age              Age-stratified SIR
#> 14                    sir_demog             SIR with Demography
#> 15                 sir_mosquito            Mosquito-Vector  SIR
#> 16                   sir_waning SIR with Waning Immunity (SIRS)
#> 17                           ww                Wastewater Model
#>                                                                                                        Description
#> 1                                                                     Behaviour modifications in response to death
#> 2                                                     Matrix population model for generating the Fibonacci numbers
#> 3                                                                                               A simple HIV model
#> 4                                                                             Simple two-species competition model
#> 5                                                                                       Simple predator-prey model
#> 6                                                         Re-implementation of the McMaster group's COVID-19 model
#> 7  An ecological model using population genomics to design optimal vaccines as implemented in Colijn et al. (2020)
#> 8                                                                      Simple epidemic model with an exposed class
#> 9                                                       A modified SEIR model with Hospitalization and Vaccination
#> 10                                                                                    A very simple epidemic model
#> 11                        SI model with behavioural protection in response to the number of infectious individuals
#> 12                                                                                    A very simple epidemic model
#> 13                                                                                     An age-stratified SIR model
#> 14                                                                               An SIR model with birth and death
#> 15                                                                                  SIR model for mosquito vectors
#> 16                                                                  A basic SIR model with a flow from R back to S
#> 17                                                             Macpan base with an additional wastewater component
```
