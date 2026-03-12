# Fitting Distributional Parameters

Distributional parameters can be added to the list of parameters that
are fit during calibration. By default, distributional parameters in
priors and likelihoods are not fit. Use `mp_nofit` to exclude
distributional parameters from being fit and `mp_fit` to fit them.

## Usage

``` r
mp_fit(x, trans = DistrParamTransDefault())

mp_nofit(x, trans = DistrParamTransDefault())
```

## Arguments

- x:

  numeric starting value of the distributional parameter to fit, or
  character name of an existing variable in the model with a default
  starting value to use.

- trans:

  transformation to apply to the distributional parameter. By default,
  distributional parameters inherit a default transformation from the
  associated distribution. For example, the standard deviation parameter
  `sd` in the
  [`mp_normal`](https://canmod.github.io/macpan2/reference/distribution.md)
  distributions has a default log transformation specified using
  [`mp_log`](https://canmod.github.io/macpan2/reference/transform_distr_param.md).

## Value

A distributional parameter object.

## Examples

``` r
# First we call the SIR model spec, and generate some data for calibration.
spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
data = mp_simulator(spec, 50, "infection") |> mp_trajectory()

# Suppose we want to specify a Normal prior on the transmission parameter 
# beta, and we are interested in estimating the prior standard deviation.
# Here we use `mp_fit` to estimate the standard deviation, `sd`, and we 
# provide a numeric starting value for `sd` in the optimization. 
cal = mp_tmb_calibrator(
    spec
  , data
  , traj = "infection"
  , par = list(beta = mp_normal(location = 0.35, sd = mp_fit(0.1)))
  , default = list(beta = 0.25)
)
#> Warning: 'mp_normal()' is deprecated. Please use 'mp_norm()', instead

# When viewing the calibration objective function we can see the additional
# prior density term added for beta. The standard deviation parameter has
# been automatically named 'distr_params_log_sd_beta'.
cal$simulator$tmb_model$obj_fn$obj_fn_expr
#> ~-sum(dpois(obs_infection, clamp(sim_infection))) - sum(dnorm(beta, 
#>     0.35, exp(distr_params_log_sd_beta)))
#> <environment: 0x5570e51e6348>

# Next we optimize and view the fitted parameters. We can see the 
# distributional parameter in the coefficient table with a default value 
# equal to the numeric value we provided to `mp_fit` above.
mp_optimize(cal)
#> $par
#>     params     params 
#>  0.2005665 -1.9009040 
#> 
#> $objective
#> [1] 49.2679
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 5
#> 
#> $evaluations
#> function gradient 
#>        6        6 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
if (suppressPackageStartupMessages(require(broom.mixed))) {
  print(mp_tmb_coef(cal))
}
#>       term                  mat row col default  type  estimate   std.error
#> 1   params                 beta   0   0    0.25 fixed 0.2005665 0.009251899
#> 2 params.1 distr_params_sd_beta   0   0    0.10 fixed 0.1494335 0.106069656

# If instead we want control over the name of the new fitted distributional
# parameter, we can add a new variable to our model specification with the 
# default value set to the desired optimization starting value.
updated_spec = spec |> mp_tmb_insert(default = list(sd_var = 0.1))

# In the calibrator, we use the name of this newly added variable, "sd_var",
# as input to `mp_fit`.
cal = mp_tmb_calibrator(
    updated_spec
  , data
  , traj = "infection"
  , par = list(beta = mp_normal(location = 0.35, sd = mp_fit("sd_var")))
  , default = list(beta = 0.25)
)
#> Warning: 'mp_normal()' is deprecated. Please use 'mp_norm()', instead

# We can see this distributional parameter get propogated to the objective 
# function and the fitted parameter table.
cal$simulator$tmb_model$obj_fn$obj_fn_expr
#> ~-sum(dpois(obs_infection, clamp(sim_infection))) - sum(dnorm(beta, 
#>     0.35, exp(sd_var)))
#> <environment: 0x5570daf8b970>
mp_optimize(cal)
#> $par
#>     params     params 
#>  0.2005665 -1.9009040 
#> 
#> $objective
#> [1] 49.2679
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 6
#> 
#> $evaluations
#> function gradient 
#>        7        7 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
if (suppressPackageStartupMessages(require(broom.mixed))) {
  print(mp_tmb_coef(cal))
}
#>       term    mat row col default  type   estimate   std.error
#> 1   params   beta   0   0    0.25 fixed  0.2005665 0.009251899
#> 2 params.1 sd_var   0   0    0.10 fixed -1.9009040 0.709811879
```
