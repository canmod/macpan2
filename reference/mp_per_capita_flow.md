# Specify Flow Into, Out Of, and Between Compartments

Specify different kinds of flows into, out of, and between compartments.

## Usage

``` r
mp_per_capita_flow(from, to, rate, flow_name = NULL, abs_rate = NULL)

mp_per_capita_inflow(from, to, rate, flow_name = NULL, abs_rate = NULL)

mp_per_capita_outflow(from, rate, flow_name = NULL, abs_rate = NULL)

mp_inflow(to, rate, flow_name = NULL, abs_rate = NULL)

mp_outflow(from, rate, flow_name = NULL, abs_rate = NULL)
```

## Arguments

- from:

  String giving the name of the compartment from which the flow
  originates.

- to:

  String giving the name of the compartment to which the flow is going.

- rate:

  String giving the expression for the per-capita flow rate.
  Alternatively, and for back compatibility, a two-sided formula with
  the left-hand-side giving the name of the absolute flow rate per
  time-step and the right-hand-side giving an expression for the
  per-capita rate of flow from `from` to `to`.

- flow_name:

  String giving the name of the flow

- abs_rate:

  Deprecated synonym for `flow_name`. Please use `flow_name` in all
  future work.

## Details

The examples below can be mixed and matched in
[`mp_tmb_model_spec()`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
to produce compartmental models. The symbols used below must be used in
an appropriate context (e.g., if `N` is used for total population size,
then there must be an expression like `N ~ S + I + R` somewhere in the
model or for models with constant population size there must be a
default variable, `N`, with a numerical value).

## Functions

- `mp_per_capita_inflow()`: Only flow into the `to` compartment, and do
  not flow out of the `from` compartment. The `from` compartment can
  even be a function of a set of compartments, because it will not be
  updated. A common construction is
  `mp_per_capita_inflow("N", "S", "birth_rate", "birth")` for adding a
  birth process, which involves the total population size, `N`, rather
  than a single compartment.

- `mp_per_capita_outflow()`: Only flow out of the `from` compartment,
  without going anywhere. This is useful for removing individuals from
  the system (e.g., death). To keep track of the total number of dead
  individuals one can use `mp_per_capita_flow` and set `to` to be a
  compartment for these individuals (e.g., `to = "D"`).

- `mp_inflow()`: Only flow into the `to` compartment. For adding a birth
  or immigration process.

- `mp_outflow()`: Only flow out of the `from` compartment. For adding an
  absolute removal process that goes to 'nowhere': dangerous! The reason
  it is dangerous is that this flow can easily lead to negative values
  of state variables when the `rate` is high relative to the size of the
  `from` compartment. Often `mp_per_capita_outflow` will be a better
  choice, given that the size of the outflow will be scaled to the size
  of the `from` compartment by measuring rates on a per-capita basis.

## See also

[`mp_absolute_flow()`](https://canmod.github.io/macpan2/reference/mp_absolute_flow.md)

## Examples

``` r
# infection by mass action
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/si
mp_per_capita_flow("S", "I", "beta * I / N", "infection")
#> From: S
#> To: I
#> Per-capita rate expression: beta * I/N
#> Absolute rate symbol: infection

# recovery
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir
mp_per_capita_flow("I", "R", "gamma", "recovery")
#> From: I
#> To: R
#> Per-capita rate expression: gamma
#> Absolute rate symbol: recovery

# disease progression with different severity
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/macpan_base
mp_per_capita_flow("E", "I_mild", "alpha * phi"      , "progression_mild")
#> From: E
#> To: I_mild
#> Per-capita rate expression: alpha * phi
#> Absolute rate symbol: progression_mild
mp_per_capita_flow("E", "I_sev" , "alpha * (1 - phi)", "progression_sev")
#> From: E
#> To: I_sev
#> Per-capita rate expression: alpha * (1 - phi)
#> Absolute rate symbol: progression_sev

# birth
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir_demog
mp_per_capita_inflow("N", "S", "nu", "birth")
#> From: N
#> To: S
#> Per-capita rate expression: nu
#> Absolute rate symbol: birth

# death
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir_demog
mp_per_capita_outflow("S", "mu", "death_S")
#> From: S
#> Per-capita rate expression: mu
#> Absolute rate symbol: death_S
mp_per_capita_outflow("I", "mu", "death_I")
#> From: I
#> Per-capita rate expression: mu
#> Absolute rate symbol: death_I
mp_per_capita_outflow("R", "mu", "death_R")
#> From: R
#> Per-capita rate expression: mu
#> Absolute rate symbol: death_R

# vaccination 
# https://github.com/canmod/macpan2/blob/main/inst/starter_models/shiver
mp_per_capita_flow("S", "V", "(vrate * S/(vrate + S))/S", "vaccination")
#> From: S
#> To: V
#> Per-capita rate expression: (vrate * S/(vrate + S))/S
#> Absolute rate symbol: vaccination

# importation
# mp_inflow("I", "delta", "importation")
```
