# Dynamic Model

This is an 'old' model specification function that was tested out at a
workshop. Currently it still drives the engine-agnostic-grammar
vignette, but we plan to replace this function with
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
and other model specification functions.

## Usage

``` r
mp_dynamic_model(
  expr_list = ExprList(),
  ledgers = list(),
  init_vecs = list(),
  unstruc_mats = list()
)
```

## Arguments

- expr_list:

  Expression list.

- ledgers:

  Ledgers.

- init_vecs:

  Initial structured vectors.

- unstruc_mats:

  Initial unstructured matrices.
