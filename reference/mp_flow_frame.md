# Data Frame Describing Compartmental Model Flows

Get a data frame where each row represents a flow in a model
specification.

## Usage

``` r
mp_flow_frame(spec, topological_sort = TRUE, loops = "^$")
```

## Arguments

- spec:

  A
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- topological_sort:

  Should the states be topologically sorted to respect the main
  direction of flow?

- loops:

  Pattern for matching the names of flows that make the flow model not a
  DAG, which is a critical assumption when topologically sorting the
  order of states and flows in the output. This is only relevant if
  `topological_sort` is used.

## Value

A data frame that gives information provided in calls to
[`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
and
[`mp_per_capita_inflow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md).
