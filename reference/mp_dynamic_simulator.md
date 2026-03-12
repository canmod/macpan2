# TMB Simulator from Dynamic Model

This is an 'old' function that was tested out at a workshop. Currently
it still drives the engine-agnostic-grammar vignette, but we plan to
replace this function with
[`mp_simulator`](https://canmod.github.io/macpan2/reference/mp_simulator.md).

## Usage

``` r
mp_dynamic_simulator(
  dynamic_model,
  time_steps = 0L,
  vectors = NULL,
  unstruc_mats = NULL,
  mats_to_save = NULL,
  mats_to_return = NULL,
  params = OptParamsList(0),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  log_file = LogFile(),
  do_pred_sdreport = TRUE,
  tmb_cpp = "macpan2",
  initialize_ad_fun = TRUE,
  ...
)
```

## Arguments

- dynamic_model:

  Object product by
  [`mp_dynamic_model`](https://canmod.github.io/macpan2/reference/mp_dynamic_model.md).

- time_steps:

  Number of time steps to simulate.

- vectors:

  Named list of named vectors as initial values for the simulations that
  are referenced in the expression list in the dynamic model.

- unstruc_mats:

  = Named list of objects that can be coerced to numerical matrices that
  are used in the expression list of the dynamic model.

- mats_to_save:

  TODO

- mats_to_return:

  TODO

- params:

  TODO

- random:

  TODO

- obj_fn:

  TODO

- log_file:

  TODO

- do_pred_sdreport:

  TODO

- tmb_cpp:

  TODO

- initialize_ad_fun:

  TODO

- ...:

  TODO
