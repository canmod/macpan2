# Transform a TMB Model Specification to Account for Reporting Bias

A version of
[`mp_tmb_insert`](https://canmod.github.io/macpan2/reference/mp_tmb_insert.md)
making it more convenient to transform an incidence variable into a
reports variable, which accounts for reporting delays and
under-reporting. This new reports variable is a convolution of the
simulation history of an incidence variable with a kernel that is
proportional to a Gamma distribution of reporting delay times.

## Usage

``` r
mp_tmb_insert_reports(
  model,
  incidence_name,
  report_prob,
  mean_delay,
  cv_delay,
  reports_name = sprintf("reported_%s", incidence_name),
  report_prob_name = sprintf("%s_report_prob", incidence_name),
  mean_delay_name = sprintf("%s_mean_delay", incidence_name),
  cv_delay_name = sprintf("%s_cv_delay", incidence_name)
)
```

## Arguments

- model:

  A model produced by
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- incidence_name:

  Name of the incidence variable to be transformed.

- report_prob:

  Value to use for the reporting probability; the proportion of cases
  that get reported.

- mean_delay:

  Mean of the Gamma distribution of reporting delay times.

- cv_delay:

  Coefficient of variation of the Gamma distribution of reporting delay
  times.

- reports_name:

  Name of the new reports variable.

- report_prob_name:

  Name of the variable containing `report_prob`.

- mean_delay_name:

  Name of the variable containing `mean_delay`.

- cv_delay_name:

  Name of the variable containing `cv_delay`.
