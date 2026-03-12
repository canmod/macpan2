# Insert Log Linear Model of Time Variation (Experimental)

Insert Log Linear Model of Time Variation (Experimental)

## Usage

``` r
mp_tmb_insert_log_linear(
  model,
  parameter_name,
  design_matrices,
  time_var_parameters,
  window_names = names(time_var_parameters),
  baseline_functions = c(list(TimeVarBaselineParameter()),
    rep(list(TimeVarBaselineNumeric(0)), length(design_matrices) - 1)),
  link_functions = rep(list(mp_identity), length(design_matrices)),
  full_series_name = sprintf("time_var_output_%s", parameter_name),
  baseline_names = sprintf("baseline_%s", window_names),
  matrix_coef_names = sprintf("matrix_coef_%s", window_names),
  matrix_row_names = sprintf("matrix_row_%s", window_names),
  matrix_col_names = sprintf("matrix_col_%s", window_names),
  linear_pred_names = sprintf("linear_pred_%s", window_names),
  time_var_names = sprintf("time_var_%s", window_names),
  time_index_name = sprintf("time_index_%s", parameter_name),
  sparsity_tolerance = 0
)
```

## Arguments

- model:

  A model specification (see
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)).

- parameter_name:

  Character string giving the name of the parameter to make
  time-varying.

- design_matrices:

  List of matrices, one for each time window, describing the model of
  time variation.

- time_var_parameters:

  Named list of parameter vectors for each window, with names giving the
  window names.

- window_names:

  Names for each window.

- baseline_functions:

  It is complicated – this is a joke – I'm tired.

- link_functions:

  List of objects representing link functions.

- full_series_name:

  Name of variable storing the full time series.

- baseline_names:

  Names of variables containing the baseline in each window.

- matrix_coef_names:

  Names of vectors containing values of the non-zero elements of the
  design matrices.

- matrix_row_names:

  Names of the vectors containing row indices of the non-zero elements
  of the design matrices.

- matrix_col_names:

  Names of the vectors containing column indices of the non-zero
  elements of the design matrices.

- linear_pred_names:

  Names of the vectors containing the linear predictors in each window.

- time_var_names:

  Names of the time-varying parameter in each window.

- time_index_name:

  Name of the index at which the time varying parameter changes.

- sparsity_tolerance:

  Make design matrix coefficients exactly zero when they are below this
  tolerance.
