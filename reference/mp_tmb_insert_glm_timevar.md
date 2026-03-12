# Insert GLM Time Variation

Insert GLM Time Variation

## Usage

``` r
mp_tmb_insert_glm_timevar(
  model,
  parameter_name,
  design_matrix,
  timevar_coef,
  link_function = mp_log,
  matrix_coef_name = sprintf("matrix_coef_%s", parameter_name),
  matrix_row_name = sprintf("matrix_row_%s", parameter_name),
  matrix_col_name = sprintf("matrix_col_%s", parameter_name),
  linear_pred_name = sprintf("linear_pred_%s", parameter_name),
  timeseries_name = sprintf("timeseries_%s", parameter_name),
  timevar_coef_name = sprintf("time_var_%s", parameter_name),
  time_index_name = sprintf("time_index_%s", parameter_name),
  sparsity_tolerance = 0,
  engine_function = c("sparse_mat_mult", "group_sums")
)
```

## Arguments

- model:

  A model specification (see
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)).

- parameter_name:

  Character string giving the name of the parameter to make
  time-varying.

- design_matrix:

  Matrix of time variation.

- timevar_coef:

  Initial coefficient matrix with the same number of rows as there are
  columns of the `design_matrix`.

- link_function:

  Link function given by functions like
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md).

- matrix_coef_name:

  Name of the vector containing values of the non-zero elements of the
  design matrix.

- matrix_row_name:

  Name of the vector containing row indices of the non-zero elements of
  the design matrix.

- matrix_col_name:

  Name of the vector containing column indices of the non-zero elements
  of the design matrix.

- linear_pred_name:

  Name of the vector containing the linear predictor.

- timeseries_name:

  Name of the vector containing the time-series of the varying
  parameter.

- timevar_coef_name:

  Name of the vector containing the time-varying parameter coefficients.

- time_index_name:

  Name of the index at which the time varying parameter changes.

- sparsity_tolerance:

  Make design matrix coefficients exactly zero when they are below this
  tolerance.

- engine_function:

  Which of two
  [`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md),
  [`sparse_mat_mult`](https://canmod.github.io/macpan2/reference/engine_functions.md)
  or
  [`group_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md),
  should be used to compute the matrix multiplication. The only reason
  to use anything other than the default is for back-compatibility with
  an existing script.
