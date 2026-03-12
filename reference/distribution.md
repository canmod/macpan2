# Distributions

Distributions which can be used to specify prior or likelihood
components in model calibration.

- Uniform Distribution (Improper), only appropriate for prior
  components - `mp_unif`

&nbsp;

- Normal Distribution - `mp_norm`

&nbsp;

- Log-Normal Distribution - `mp_log_norm`

&nbsp;

- Logit-Normal Distribution - `mp_logitnorm`

&nbsp;

- Poisson Distribution - `mp_pois`

&nbsp;

- Negative Binomial Distribution - `mp_nbinom`

## Usage

``` r
mp_unif(trans_distr_param = list())

mp_uniform(trans_distr_param = list())

mp_norm(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_log)
)

mp_normal(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_log)
)

mp_lnorm(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_identity)
)

mp_log_normal(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_identity)
)

mp_logitnorm(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_identity)
)

mp_logit_normal(
  location = mp_distr_param_null("location"),
  sd,
  trans_distr_param = list(location = mp_identity, sd = mp_identity)
)

mp_pois(
  location = mp_distr_param_null("location"),
  trans_distr_param = list(location = mp_identity)
)

mp_poisson(
  location = mp_distr_param_null("location"),
  trans_distr_param = list(location = mp_identity)
)

mp_nbinom(
  location = mp_distr_param_null("location"),
  disp,
  trans_distr_param = list(location = mp_identity, disp = mp_log)
)

mp_neg_bin(
  location = mp_distr_param_null("location"),
  disp,
  trans_distr_param = list(location = mp_identity, disp = mp_log)
)
```

## Arguments

- trans_distr_param:

  Named list of transformations for each distributional parameter. See
  [`transform_distr_param`](https://canmod.github.io/macpan2/reference/transform_distr_param.md)
  for a list of available transformations.

- location:

  Location parameter. Specifying the `location` parameter is only
  necessary when the distribution is used as a prior distribution. If it
  is used as a likelihood component the location parameter will be taken
  as the simulated variable being fitted to data, and so this `location`
  parameter should be left to the default.

- sd:

  Standard deviation parameter.

- disp:

  Dispersion parameter.

## Details

All distributional parameter arguments can be specified either as a
numeric value, a character string giving the parameter name, or a
distributional parameter object (See
[`fit_distr_params`](https://canmod.github.io/macpan2/reference/fit_distr_params.md)).
