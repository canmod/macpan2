MacPan base
================
Jennifer Freeman

- [Packages Used](#packages-used)
- [Model Specification](#model-specification)
- [States](#states)
- [Parameters](#parameters)
- [Differential Equations](#differential-equations)
- [Calibration](#calibration)
  - [Observed data prep](#observed-data-prep)
  - [Mobility Sub-Model](#mobility-sub-model)
  - [Update the Model Specification for
    Calibration](#update-the-model-specification-for-calibration)
  - [Calibration](#calibration-1)
  - [Explore Calibration](#explore-calibration)
- [Computing $\mathcal{R}_0$ with a Cohort
  Model](#computing-mathcalr_0-with-a-cohort-model)
- [References](#references)

The McMasterPandemic model ([Bolker et al. 2024](#ref-macpan)), which is
modified SEIR model that incorporates additional infectious compartments
to reflect the current knowledge of COVID-19 epidemiology. Exposed
individuals enter four infectious compartments characterized by the
individuals symptom status (asymptomatic, pre-symptomatic, mild, and
severe). Severely infected individuals require care through hospital
and/or Intensive Care Unit (ICU) compartments and either recover or die.

This model was used throughout the pandemic to support public health
responses (e.g.,
<https://mac-theobio.github.io/forecasts/outputs/ON_accuracy.html>).

# Packages Used

The code in this article uses the following packages.

``` r
library(macpan2)
library(ggplot2)
library(dplyr)
```

# Model Specification

This model has been specified in the `macpan_base` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/macpan_base/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details). We can read in the model specification using the
`mp_tmb_library` command.

``` r
spec = mp_tmb_library(
    "starter_models"
  , "macpan_base"
  , package = "macpan2"
)
```

This specification can be used to draw the following flow diagrams using
code found in the [source for this
article](https://github.com/canmod/macpan2/blob/main/inst/starter_models/macpan_base/README.Rmd).

![](./figures/diagram-1.png)<!-- -->

# States

| variable | description                                                         |
|----------|---------------------------------------------------------------------|
| $S$      | Number of susceptible individuals                                   |
| $E$      | Number of exposed individuals                                       |
| $I_a$    | Number of asymptomatic infectious individuals                       |
| $I_p$    | Number of pre-symptomatic infectious individuals                    |
| $I_m$    | Number of mildly infectious individuals                             |
| $I_s$    | Number of severely infectious individuals                           |
| $H$      | Number of hospitalized individuals (acute care)                     |
| $ICU_s$  | Number of individuals admitted to the ICU with a survival prognosis |
| $ICU_d$  | Number of individuals admitted to the ICU with a death prognosis    |
| $H_2$    | Number of hospitalized individuals (acute care) after ICU stay      |
| $D$      | Number of dead individuals                                          |
| $R$      | Number of recovered individuals                                     |

The size of the total population is,
$N = S + E + I_a + I_p + I_m + I_s + H + ICU_s + ICU_d + H_2 + D + R$.

# Parameters

| variable       | description                                                                         |
|----------------|-------------------------------------------------------------------------------------|
| $\beta_0$      | baseline (non-intervention) transmission across categories                          |
| $C_a$          | relative asymptomatic transmission (or contact) proportion                          |
| $C_p$          | relative presymptomatic transmission (or contact) proportion                        |
| $C_m$          | relative mildly transmission (or contact) proportion                                |
| $C_s$          | relative severly transmission (or contact) proportion                               |
| $\alpha$       | fraction of infections that are asymptomatic                                        |
| $\mu$          | fraction of symptomatic infections that are mild                                    |
| $\sigma$       | 1/time in exposed class                                                             |
| $\gamma_a$     | 1/time to recovery for asymptomatic infections                                      |
| $\gamma_p$     | 1/time in pre-symptomatic state                                                     |
| $\gamma_m$     | 1/time to recovery for mildly symptomatic infections                                |
| $\gamma_s$     | 1/time spent in severely symptomatic state before either hospitalization or death   |
| $\rho$         | 1/time in hospital (initial acute care admission)                                   |
| $\delta_{nh}$  | probability of mortality without hospitalization                                    |
| $\text{iso}_m$ | relative self-isolation/distancing of mild cases                                    |
| $\text{iso}_s$ | relative self-isolation/distancing of severe cases                                  |
| $\phi_1$       | fraction of hospitalized infections that only require acute care (no ICU admission) |
| $\phi_2$       | fraction of ICU infections that are fatal                                           |
| $\psi_1$       | 1/time spent in ICU before returning to acute care                                  |
| $\psi_2$       | 1/time spent in ICU before dying                                                    |
| $\psi_3$       | 1/time in post-ICU acute care before hospital discharge                             |

# Differential Equations

<!-- nb. do not remove the spaces in `\delta _ {nh}`. https://github.com/github/markup/issues/1575 -->

$$
\begin{align*}
\frac{dS}{dt} &= -\beta_0\left(C_aI_a +C_pI_p + C_mI_m(1-\text{iso}_m) + C_sI_s(1-\text{iso}_s)\right)S/N \\
\frac{dE}{dt} &= \beta_0\left(C_aI_a +C_pI_p + C_mI_m(1-\text{iso}_m) + C_sI_s(1-\text{iso}_s)\right)S/N - \sigma E \\
\frac{dI_a}{dt} &= \alpha\sigma E- \gamma_a I_a \\
\frac{dI_p}{dt} &= (1-\alpha)\sigma E- \gamma_p I_p \\
\frac{dI_m}{dt} &= \mu\gamma_pI_p- \gamma_m I_m \\
\frac{dI_s}{dt} &= (1-\mu)\gamma_pI_p- (1-\delta _ {nh})\gamma_s I_s \\
\frac{dH}{dt} &= (1-\delta _ {nh})\phi_1\gamma_s I_s - \rho H  \\
\frac{dICU_s}{dt} &= (1-\delta _ {nh})(1-\phi_1)(1-\phi_2)\gamma_s I_s - \psi_1 ICU_s \\
\frac{dICU_d}{dt} &= (1-\delta _ {nh})(1-\phi_1)\phi_2\gamma_s I_s - \psi_2 ICU_d \\
\frac{dH_2}{dt} &= \psi_1 ICU_s - \psi_3 H_2  \\
\frac{dR}{dt} &= \gamma_a I_a + \gamma_m I_m + \rho H + \psi_3 H_2 \\
\frac{dD}{dt} &=  \psi_2 ICU_d
\end{align*}
$$

When there were discrepancies with how the model is expressed in Papst
(TBD) versus model definition files, the latter was chosen.

# Calibration

## Observed data prep

We fit this model to observed death and case reports. For convenience we
have saved some Ontario COVID-19 data in the `macpan2` package.

``` r
ts_data  = readRDS(url("https://github.com/canmod/macpan2/releases/download/macpan1.5_data/covid_on.RDS"))
```

To further prepare the time series data for calibration we filter for
the appropriate time range and time series variables, create a numeric
date field named ‘time’.

``` r
prepped_ts_data = (ts_data
  |> rename(matrix = var)
  # dates from base model calibration (Figure 4)
  |> filter(date >= "2020-02-24" & date < "2020-08-31")
  # create unique time identifier
  |> arrange(date)
  |> group_by(date)
  |> mutate(time = cur_group_id())
  |> ungroup()
  # one negative value for daily deaths (removing for now time==178)
  # this explains negative values:
  # https://github.com/ccodwg/Covid19Canada?tab=readme-ov-file#datasets
  |> filter(matrix %in% c("death","report") & value >= 0)

)
```

To further prepare the mobility data for calibration we filter for the
appropriate time range, create a numeric date field named ‘time’ and
compute the logarithm of the mobility index.

``` r
prepped_mobility_data = (ts_data
  |> filter(var == "mobility_index")
  # dates from base model calibration (Figure 4)
  |> filter(date >= "2020-02-24" & date < "2020-08-31")
  # create unique time identifier
  |> arrange(date)
  |> group_by(date)
  |> mutate(time = cur_group_id())
  |> ungroup()
  |> mutate(log_mobility_ind = log(value))
)
```

Mobility breakpoints identified for piecewise varying transmission.

``` r
mobility_breaks = (prepped_ts_data
  |> filter(date %in% c("2020-04-01", "2020-08-07"), matrix=="report")
  |> pull(time)
)
```

To support code below, we define the number of time steps in the model
from the data.

``` r
time_steps = nrow(prepped_ts_data %>% select(date) %>% unique())
prepped_ts_data = select(prepped_ts_data, -date)
```

## Mobility Sub-Model

The function `S_j` computes the logistic transition curve for mobility
breakpoints, and this function is used to produce the model matrix, `X`,
describing the temporal change in transmission using mobility data and
piecewise breaks smoothed with the logistic curve.

``` r
S_j = function(t, tau_j, s) 1/(1 + exp((t - tau_j) / s))
X = cbind(
      prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[1], 3)
    , S_j(1:time_steps, mobility_breaks[1], 3) * prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[2], 3)
    , S_j(1:time_steps, mobility_breaks[2], 3) * prepped_mobility_data$log_mobility_ind
    ) %>% as.matrix()
X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)
model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index
```

## Update the Model Specification for Calibration

Update model specification to include additional components described in
manuscript and required for calibration.

``` r
focal_model = (spec 
   # add variable transformations:
   |> mp_tmb_insert(phase = "before"
      , at = 1L
      , expressions = list(
          zeta ~ exp(log_zeta)
        , beta0 ~ exp(log_beta0)
        , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
        , E ~ exp(log_E)
      )
      , default = list(
          log_zeta = empty_matrix
        , log_beta0 = empty_matrix
        , logit_nonhosp_mort = empty_matrix
        , log_E = empty_matrix
      )

   )
   
   # add accumulator variables:
   # death - new deaths each time step
   |> mp_tmb_insert(phase = "during"
      , at = Inf
      , expressions = list(death ~ ICUd.D + Is.D)
   )
   
   # add phenomenological heterogeneity:
   |> mp_tmb_update(phase = "during"
      , at =1L
      , expressions = list(
        mp_per_capita_flow(
            "S", "E"
          , "((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))"
          , "S.E"
        ))

   )
   
   # compute gamma-density delay kernel for convolution:
   |> mp_tmb_insert(phase = "before"
      , at = Inf
      , expressions = list(
          gamma_shape ~ 1 / (c_delay_cv^2)
        , gamma_scale ~ c_delay_mean * c_delay_cv^2
        , gamma ~ pgamma(1:(qmax+1), gamma_shape, gamma_scale)
        , delta ~ gamma[1:(qmax)] - gamma[0:(qmax - 1)]
        , kappa ~ c_prop * delta / sum(delta)
      )
      , default = list(qmax = empty_matrix)
   )
   
   # add convolution to compute case reports from incidence:
   |> mp_tmb_insert(phase = "during"
      , at = Inf
      , expressions = list(report ~ convolution(S.E, kappa))
   )
   
   # add time-varying transmission with mobility data:
   |> mp_tmb_insert(phase = "before"
      , at = Inf
      , expressions = list(relative_beta_values ~ group_sums(model_matrix_values * log_mobility_coefficients[col_ind], row_ind, model_matrix_values))
      , default = list(log_mobility_coefficients = empty_matrix, model_matrix_values = empty_matrix)
      , integers = list(row_ind = row_ind, col_ind = col_ind)
   )
   |> mp_tmb_insert(phase = "during"
      , at = 1L
      , expressions = list(
        beta ~ exp(log_beta0 + relative_beta_values[time_step(1)])
      )
   )

)
```

## Calibration

``` r
focal_calib = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = prepped_ts_data
  , traj = c("report","death") 
  , par = c(
      "log_zeta"
    , "log_beta0"
    , "logit_nonhosp_mort"
    , "log_mobility_coefficients"
    , "log_E" 
    # negative binomial dispersion parameters for reports and deaths get added
    # automatically with options(macpan2_default_loss = "neg_bin") set above
  )
  , outputs = c("death","report")
  , default = list(
    
    # states
    # Population of Ontario (2019) from:
    # https://github.com/mac-theobio/macpan_base/blob/main/code/ontario_calibrate_comb.R
      S = 14.57e6 - 5 
    , log_E = log(5)
    , Ia = 0
    , Ip = 0
    , Im = 0
    , Is = 0
    , R = 0
    , H = 0
    , ICUs = 0
    , ICUd = 0
    , H2 = 0
    , D = 0
    
    , model_matrix_values = model_matrix_values
    
    # set initial parameter values for optimizer
    
    # width of convolution kernel computed according to:
    # https://canmod.net/misc/flex_specs#computing-convolutions
    # shape = 1/(mp_default_list(focal_model)$c_delay_cv^2)
    # scale = mp_default_list(focal_model)$c_delay_mean * mp_default_list(focal_model)$c_delay_cv^2
    # qmax = ceiling(qgamma(0.95, shape, scale))
    , qmax = 34
    , log_beta0=log(5)
    , logit_nonhosp_mort = -0.5
    , log_mobility_coefficients = rep(0,5)
    , log_zeta = 1
    
  )
)
# converges
mp_optimize(focal_calib)
#> outer mgc:  464198.9 
#> outer mgc:  206291.3 
#> outer mgc:  277399.9 
#> outer mgc:  123919.2 
#> outer mgc:  62669.62 
#> outer mgc:  43175.41 
#> outer mgc:  12045.94 
#> outer mgc:  2009.163 
#> outer mgc:  4699.982 
#> outer mgc:  1280.61 
#> outer mgc:  1959.389 
#> outer mgc:  2197.84 
#> outer mgc:  2598.58 
#> outer mgc:  1801.487 
#> outer mgc:  1683.596 
#> outer mgc:  837.0336 
#> outer mgc:  1238.395 
#> outer mgc:  250.4317 
#> outer mgc:  422.8622 
#> outer mgc:  396.0051 
#> outer mgc:  295.646 
#> outer mgc:  263.7446 
#> outer mgc:  184.0072 
#> outer mgc:  235.371 
#> outer mgc:  140.5446 
#> outer mgc:  420.7473 
#> outer mgc:  76.19063 
#> outer mgc:  191.1308 
#> outer mgc:  191.0764 
#> outer mgc:  205.1883 
#> outer mgc:  252.5784 
#> outer mgc:  230.8998 
#> outer mgc:  252.3025 
#> outer mgc:  214.5108 
#> outer mgc:  240.2282 
#> outer mgc:  194.4268 
#> outer mgc:  420.4915 
#> outer mgc:  107.8389 
#> outer mgc:  724.2023 
#> outer mgc:  27.78877 
#> outer mgc:  120.7651 
#> outer mgc:  610.5989 
#> outer mgc:  34.03929 
#> outer mgc:  128.7617 
#> outer mgc:  434.6105 
#> outer mgc:  53.35816 
#> outer mgc:  771.0387 
#> outer mgc:  10.57253 
#> outer mgc:  137.1906 
#> outer mgc:  168.7642 
#> outer mgc:  214.5904 
#> outer mgc:  79.08095 
#> outer mgc:  97.2613 
#> outer mgc:  93.59715 
#> outer mgc:  263.6426 
#> outer mgc:  33.36824 
#> outer mgc:  41.497 
#> outer mgc:  161.355 
#> outer mgc:  64.72287 
#> outer mgc:  96.64981 
#> outer mgc:  91.75068 
#> outer mgc:  85.95648 
#> outer mgc:  81.80533 
#> outer mgc:  76.08382 
#> outer mgc:  72.38908 
#> outer mgc:  66.87035 
#> outer mgc:  63.374 
#> outer mgc:  58.28888 
#> outer mgc:  54.72251 
#> outer mgc:  134.8422 
#> outer mgc:  22.33034 
#> outer mgc:  22.57781 
#> outer mgc:  87.00417 
#> outer mgc:  38.15251 
#> outer mgc:  39.31674 
#> outer mgc:  127.3303 
#> outer mgc:  12.68002 
#> outer mgc:  12.78859 
#> outer mgc:  49.14113 
#> outer mgc:  45.76847 
#> outer mgc:  44.74042 
#> outer mgc:  40.74307 
#> outer mgc:  40.88682 
#> outer mgc:  35.85751 
#> outer mgc:  37.42011 
#> outer mgc:  31.24157 
#> outer mgc:  34.00924 
#> outer mgc:  27.18883 
#> outer mgc:  30.21041 
#> outer mgc:  23.94069 
#> outer mgc:  25.88689 
#> outer mgc:  21.32554 
#> outer mgc:  61.35206 
#> outer mgc:  7.869171 
#> outer mgc:  7.579318 
#> outer mgc:  28.53918 
#> outer mgc:  24.77954 
#> outer mgc:  23.61663 
#> outer mgc:  22.35336 
#> outer mgc:  21.30244 
#> outer mgc:  20.01761 
#> outer mgc:  19.10098 
#> outer mgc:  17.74856 
#> outer mgc:  16.97831 
#> outer mgc:  15.5398 
#> outer mgc:  14.87941 
#> outer mgc:  13.40578 
#> outer mgc:  12.74096 
#> outer mgc:  11.35231 
#> outer mgc:  10.54555 
#> outer mgc:  24.06877 
#> outer mgc:  4.931889 
#> outer mgc:  4.499848 
#> outer mgc:  15.91305 
#> outer mgc:  6.829861 
#> outer mgc:  5.982218 
#> outer mgc:  23.24753 
#> outer mgc:  1.992496 
#> outer mgc:  1.816683 
#> outer mgc:  6.413924 
#> outer mgc:  5.799307 
#> outer mgc:  4.79566 
#> outer mgc:  4.02995 
#> outer mgc:  10.74125 
#> outer mgc:  1.882369 
#> outer mgc:  1.569593 
#> outer mgc:  1.308247 
#> outer mgc:  1.090408 
#> outer mgc:  3.430568 
#> outer mgc:  2.382979 
#> outer mgc:  1.655393 
#> outer mgc:  1.132065 
#> outer mgc:  2.520275 
#> outer mgc:  0.6971995 
#> outer mgc:  0.4287998 
#> outer mgc:  0.2637154 
#> outer mgc:  0.4919979 
#> outer mgc:  0.1730144
#> $par
#>      params      params      params      params      params      params 
#> -11.8661427  -1.6061443  -1.3693963  -8.9894233   1.5250569   0.4485781 
#>      params      params      params 
#>  -0.5637307   8.6786877   4.9272458 
#> 
#> $objective
#> [1] 2527.996
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 137
#> 
#> $evaluations
#> function gradient 
#>      166      138 
#> 
#> $message
#> [1] "relative convergence (4)"
```

## Explore Calibration

We can get the trajectory and confidence intervals.

``` r
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)
#> outer mgc:  0.1730144 
#> outer mgc:  0.1732991 
#> outer mgc:  0.1727301 
#> outer mgc:  5277.764 
#> outer mgc:  5217.212 
#> outer mgc:  17.18549 
#> outer mgc:  17.52086 
#> outer mgc:  2130.696 
#> outer mgc:  2140.877 
#> outer mgc:  2724.153 
#> outer mgc:  2708.907 
#> outer mgc:  725.9018 
#> outer mgc:  726.8065 
#> outer mgc:  5232.033 
#> outer mgc:  5172.877 
#> outer mgc:  2130.627 
#> outer mgc:  2140.807 
#> outer mgc:  425.823 
#> outer mgc:  425.8619
```

``` r
# View estimates and their confidence intervals:
# zeta ~ 0(wide CI), do we need phenomenological heterogeneity?
# beta0 ~ 0.18, order of magnitude smaller than manuscript estimate (Table 1)
# mobility coefficients:
#   mobility power                                 ~ 3.6 (huge std err)
#   relative change in transmission (breakpoint 1) ~ 3.9 (3.4,4.6)
#   change in mobility power (breakpoint 1)        ~ 1.04 (0.8,1.4)
#   relative change in transmission (breakpoint 2) ~ 0.64 (0.56,0.74)
#   change in mobility power (breakpoint 2)        ~ 0.21 (wide CI)
# E(0) ~ 173(102,295) plausible for early pandemic (but different from manuscript)
# nohosp_mort ~ 0.14(0.11,0.16) is this too high?
# theta_report ~ 3(2.8,3.3) very different from manuscript
# theta_death ~ 0.5(0.2,0.8) very different from manuscript
mp_tmb_coef(focal_calib, conf.int = TRUE)
#> outer mgc:  0.1730144 
#> outer mgc:  0.1732991 
#> outer mgc:  0.1727301 
#> outer mgc:  5277.764 
#> outer mgc:  5217.212 
#> outer mgc:  17.18549 
#> outer mgc:  17.52086 
#> outer mgc:  2130.696 
#> outer mgc:  2140.877 
#> outer mgc:  2724.153 
#> outer mgc:  2708.907 
#> outer mgc:  725.9018 
#> outer mgc:  726.8065 
#> outer mgc:  5232.033 
#> outer mgc:  5172.877 
#> outer mgc:  2130.627 
#> outer mgc:  2140.807 
#> outer mgc:  425.823 
#> outer mgc:  425.8619 
#> outer mgc:  4153.555
#>       term                   mat row col   default  type     estimate
#> 1   params                  zeta   0   0 2.7182818 fixed 7.024246e-06
#> 2 params.1                 beta0   0   0 5.0000000 fixed 2.006598e-01
#> 4 params.3 mobility_coefficients   0   0 1.0000000 fixed 1.247220e-04
#> 5 params.4 mobility_coefficients   1   0 1.0000000 fixed 4.595405e+00
#> 6 params.5 mobility_coefficients   2   0 1.0000000 fixed 1.566084e+00
#> 7 params.6 mobility_coefficients   3   0 1.0000000 fixed 5.690820e-01
#> 8 params.7 mobility_coefficients   4   0 1.0000000 fixed 5.876330e+03
#> 9 params.8                     E   0   0 5.0000000 fixed 1.379989e+02
#> 3 params.2          nonhosp_mort   0   0 0.3775407 fixed 2.027174e-01
#>      std.error     conf.low    conf.high
#> 1 1.645674e-02 2.220446e-16          Inf
#> 2 4.586122e-03 1.918695e-01 2.098528e-01
#> 4 4.052456e-04 2.139078e-07 7.272095e-02
#> 5 2.191076e-01 4.185417e+00 5.045554e+00
#> 6 1.134402e-01 1.358807e+00 1.804980e+00
#> 7 1.496536e-02 5.404935e-01 5.991827e-01
#> 8 1.907115e+04 1.015314e+01 3.401041e+06
#> 9 2.625957e+01 9.503904e+01 2.003777e+02
#> 3 5.228220e-03 1.926632e-01 2.131578e-01
```

``` r
(ggplot(prepped_ts_data, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data |> filter(matrix %in% c("death","report"))
              , colour = "red"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data |> filter(matrix %in% c("death","report"))
                , alpha = 0.2
                , colour = "red"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)
```

![](./figures/plot_fit-1.png)<!-- -->

# Computing $\mathcal{R}_0$ with a Cohort Model

The following is some advanced material that we are planning to include
as a feature for calculating $\mathcal{R}_0$ in the package for

Simulate a single exposed individual through time (N=1, E=1, all other
states 0). The transmission kernel is the force of infection at each
time step of the simulation. We sum the transmission kernel to get an
estimate on $\mathcal{R}_0$ (for a given set of parameters?)

In our case, our focal model includes phenomenological heterogeneity and
setting S to 0, leads to 0^0 issues. SW said: “The way out, I think, is
to realize that the kernel method assumes we are at the beginning of the
epidemic and therefore that S/N ~ 1. In this case, FOI reduces to beta
\* I / N, which presents no issue. So we do not need safe_power.”

Update focal model model to remove inflow to E (ensuring no new
susceptibles reach E). This update also automatically turns off outflow
from S, which is also what we want because this makes S/N ~ 1. In this
case we don’t initialize S to 0. We have N = N_focal (full population),
`N_cohort` = cohort population (1 in this case).

``` r
cohort_model_ph = (
  focal_model
  %>% mp_tmb_update(phase="during"
                    , at = 2L
                    , expressions = list(S.E ~ (S^zeta) * (beta / (N^(zeta) * N_cohort)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
                    , default = list(S.E = 0))
)
```

This cohort model allows us to simulate the cohort.

``` r
cohort_sim_ph = (mp_simulator(cohort_model_ph
                  , time_steps = 100L
                  , outputs = "S.E"
                  , default = list(
                      S = 14.57e6 - 1 
                    , log_E = log(1)
                    , Ia = 0
                    , Ip = 0
                    , Im = 0
                    , Is = 0
                    , R = 0
                    , H = 0
                    , ICUs = 0
                    , ICUd = 0
                    , H2 = 0
                    , D = 0
                    , N_cohort = 1
                    , model_matrix_values = model_matrix_values
                    , qmax = 34
                    , log_beta0=log(1)
                    , logit_nonhosp_mort = -0.5
                    , log_mobility_coefficients = rep(0,5)
                    , log_zeta = 1
                    )
  ) |> mp_trajectory()
) 
```

If we ran our simulations for long enough, the sum of these simulations
is $\mathcal{R}_0$.

``` r
R0_ph = sum(cohort_sim_ph$value)
print(R0_ph)
#> [1] 6.787641
```

Should we turn off phenomenological heterogeneity, or turn off inflow to
E? If so, update S.E flow to remove phenomenological heterogeneity add
duplicate foi expression because we can’t recover the foi = S.E / S,
when S is 0. In this case we do initialize S to 0.

``` r
cohort_model = (focal_model
  %>% mp_tmb_update(phase="during"
                    , at = 2L
                    , expressions = list(mp_per_capita_flow(
        "S", "E"
      , "(beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs * (1 - iso_s))"
      , "S.E" 
    )
  ))
  %>% mp_tmb_insert(phase="during"
                    , at = Inf
                    , expressions = list(foi ~ (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
  )
)

cohort_sim = (mp_simulator(cohort_model
                           , time_steps = 100L
                           , outputs = "foi"
                           , default = list(
                               S = 0
                             , log_E = log(1)
                             , Ia = 0
                             , Ip = 0
                             , Im = 0
                             , Is = 0
                             , R = 0
                             , H = 0
                             , ICUs = 0
                             , ICUd = 0
                             , H2 = 0
                             , D = 0
                             , N = 1
                             , model_matrix_values = model_matrix_values
                             , qmax = 34
                             , log_beta0=log(1)
                             , logit_nonhosp_mort = -0.5
                             , log_mobility_coefficients = rep(0,5)
                             , log_zeta = 1) # not actually used
  ) |> mp_trajectory()
) 

R0 = sum(cohort_sim$value)
print(R0)
#> [1] 6.787643
```

It turns out that we arrive at approximately the same answer with both
approaches.

``` r
all.equal(R0_ph, R0)
#> [1] "Mean relative difference: 2.408819e-07"
```

To solve for intrinsic growth rate `r`, we find the root of the
Euler-Lotka equation.

``` r
euler_lotka = function(r) sum(cohort_sim$value * exp(-r * cohort_sim$time)) - 1
uniroot(euler_lotka, c(0,10))
#> $root
#> [1] 0.3485261
#> 
#> $f.root
#> [1] -1.617883e-05
#> 
#> $iter
#> [1] 11
#> 
#> $init.it
#> [1] NA
#> 
#> $estim.prec
#> [1] 6.103516e-05
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-macpan" class="csl-entry">

Bolker, Ben, David Earn, Morgan Kain, Mike Li, and Jonathan Dushoff.
2024. *McMasterPandemic: Pandemic Model*.
<https://github.com/bbolker/McMasterPandemic>.

</div>

</div>
