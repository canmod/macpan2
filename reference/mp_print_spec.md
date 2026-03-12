# Print Model Specification

Print Model Specification

## Usage

``` r
mp_print_spec(model)

mp_print_before(model)

mp_print_during(model)

mp_print_after(model)
```

## Arguments

- model:

  A model produced by
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

## Functions

- `mp_print_before()`: Print just the expressions executed before the
  simulation loop.

- `mp_print_during()`: Print just the expressions executed during each
  iteration of the simulation loop.

- `mp_print_after()`: Print just the expressions executed after the
  simulation loop.
