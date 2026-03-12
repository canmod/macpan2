# Dynamic Variable Names

Get the state, flow, and other dynamic variables in a model
specification. Dynamic variables are any variable that is updated every
time step. State and flow variables are special dynamic variables
involved in compartmental models that have been explicitly represented
using functions like
[`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
that define flows among compartments (i.e., states).

## Usage

``` r
mp_state_vars(spec, topological_sort = FALSE, loops = "^$", trans = "")

mp_flow_vars(spec, topological_sort = FALSE, loops = "^$", trans = "")

mp_state_flow_vars(spec, topological_sort = FALSE, loops = "^$", trans = "")

mp_dynamic_vars(spec)

mp_other_dynamic_vars(spec)
```

## Arguments

- spec:

  Model specification
  ([`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)).

- topological_sort:

  Should the states and flows be [topologically
  sorted](https://en.wikipedia.org/wiki/Topological_sorting) to respect
  the main direction of flow? The default is no topological sorting,
  which differs from
  [`mp_flow_frame`](https://canmod.github.io/macpan2/reference/mp_flow_frame.md).

- loops:

  Pattern for matching the names of flows that make the flow model not a
  DAG, which is a critical assumption when topologically sorting the
  order of states and flows in the output. This is only relevant if
  `topological_sort` is used.

- trans:

  Add a prefix to the names for indicating if a transformed version of
  the variables is preferred.

## Value

Character vector of names of all requested variables.

## Details

State and flow variables will be identical regardless of the state
update method (e.g.,
[`mp_rk4`](https://canmod.github.io/macpan2/reference/state_updates.md)),
but other dynamic variables might appear for one particular state update
method that does not appear for another. For example, the first Runge
Kutta step for a state in a model with an
[`mp_rk4`](https://canmod.github.io/macpan2/reference/state_updates.md)
updater, will not appear with an
[`mp_euler`](https://canmod.github.io/macpan2/reference/state_updates.md)
updater.

## Functions

- `mp_state_vars()`: Return character vector of all state variables.

- `mp_flow_vars()`: Return the names of all variables that contain the
  absolute flow between compartments. The absolute flow is the magnitude
  of a flow per time step.

- `mp_state_flow_vars()`: Union of `mp_state_vars()` and
  `mp_flow_vars()`.

- `mp_dynamic_vars()`: All variables that are updated once per
  time-step.

- `mp_other_dynamic_vars()`: All variables that are updated once per
  time-step, excluding those that are state and flow variables.

## Examples

``` r
si = mp_tmb_library("starter_models", "si", package = "macpan2")
(si
  |> mp_simulator(time_steps = 5L, mp_state_vars(si))
  |> mp_trajectory()
)
#>    matrix time row col     value
#> 1       I    1   0   0  1.198000
#> 2       S    1   0   0 98.802000
#> 3       I    2   0   0  1.434730
#> 4       S    2   0   0 98.565270
#> 5       I    3   0   0  1.717559
#> 6       S    3   0   0 98.282441
#> 7       I    4   0   0  2.055170
#> 8       S    4   0   0 97.944830
#> 9       I    5   0   0  2.457757
#> 10      S    5   0   0 97.542243
```
