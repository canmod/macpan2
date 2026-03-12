# Adjacency Matrix

Get the [adjacency
matrix](https://en.wikipedia.org/wiki/Adjacency_matrix) associated with
a model specification.

## Usage

``` r
mp_adjacency(spec, include_inout = FALSE)
```

## Arguments

- spec:

  A model specification object (for example, created using
  [`mp_tmb_model_spec()`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)).

- include_inout:

  (logical) include nodes defined by
  [`mp_per_capita_inflow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
  and
  [`mp_per_capita_outflow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)?

## Value

An adjacency
[`matrix`](https://canmod.github.io/macpan2/reference/engine_functions.md).
