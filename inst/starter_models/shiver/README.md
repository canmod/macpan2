SHIVER = SEIR + H + V
================
Jennifer Freeman

- [States](#states)
- [Parameters](#parameters)
  - [Variable Vaccination Rate](#variable-vaccination-rate)
- [Dynamics](#dynamics)
- [Calibration Example](#calibration-example)
  - [Calibration Scenario](#calibration-scenario)
  - [Deciding on Defaults](#deciding-on-defaults)
  - [Simulating Dynamics](#simulating-dynamics)
  - [Estimating Parameters](#estimating-parameters)
  - [Re-parameterizing and Introducing
    Transformations](#re-parameterizing-and-introducing-transformations)
  - [Runge-Kutta 4](#runge-kutta-4)
  - [Fitting to Multiple
    Trajectories](#fitting-to-multiple-trajectories)
  - [Parameter Identifiability](#parameter-identifiability)
- [Model Specification](#model-specification)
- [References](#references)

This model builds on the basic SEIR model, with two additional
compartments for vaccination and hospitalizations.

Vaccines are typically subject to resource constraints and distribution
strategies might prioritize vaccinations for specific subpopulations,
such as immunocompromised people, to reduce bad outcomes. We model this
with a flow of susceptibles entering the vaccination class. This flow
could be a fixed rate, i.e. a constant proportion of the population
receives a vaccine each time step, but instead we wish to capture a more
realistic vaccination rate by allowing it to vary (see [Variable
Vaccination Rate](#variable-vaccination-rate)).

Following vaccination, most individuals return to the susceptible class
when the acquired immune response wears off, called *vaccine waning*.

In reality, vaccinations are only partly effective in developing a
protective host immune response. This *vaccine failure* includes both
vaccine-related inadequacies and host-related reasons like health status
and genetic factors ([Wiedermann, Garner-Spitzer, and Wagner
2016](#ref-wiedermann_2016)). Vaccine failure is modelled as flow from
vaccination to the exposed class.

The SHIVER model assumes all individuals can be exposed to the disease,
but transmission rates depend on vaccination status. Additionally,
individuals with severe infections are hospitalized and assumed to be
isolated, before recovering from the disease. Hospital isolation means
this portion of infectious individuals no longer contribute to the
transmission dynamics.

# States

| variable | description                        |
|----------|------------------------------------|
| S        | Number of susceptible individuals  |
| H        | Number of hospitalized individuals |
| I        | Number of infectious individuals   |
| V        | Number of vaccinated individuals   |
| E        | Number of exposed individuals      |
| R        | Number of recovered individuals    |

The size of the total population is, $N = S + H + I + V + E + R$, and
the disease spreads through homogeneous mixing of the subpopulation
$N_{\text{mix}}=N -H$.

# Parameters

| variable   | description                                                                                         |
|------------|-----------------------------------------------------------------------------------------------------|
| $\phi$     | per capita vaccination rate of susceptibles                                                         |
| $\rho$     | per capita vaccine waning rate                                                                      |
| $\beta_S$  | per capita transmission rate for susceptibles (in $N_{\text{mix}}$ population)                      |
| $\beta_V$  | per capita transmission rate for vaccinated individuals (in $N_{\text{mix}}$ population)            |
| $\alpha$   | per capita infection rate (average time spent in compartment $E$ is $1/\alpha$)                     |
| $\gamma_I$ | per capita recovery rate for infected individuals                                                   |
| $\gamma_H$ | per capita recovery rate for hospitalized individuals                                               |
| $\sigma$   | per capita rate at which infected individuals develop severe infections and require hospitalization |

## Variable Vaccination Rate

We can implement vaccine constraints by adding more model complexity.
Resource limitations create an upper bound on the number of vaccines
that can be administered to susceptibles per time step. There is also
the constraint that we can only vaccinate, at most, the current number
of susceptibles i.e. the vaccination rate can be at most 1. These
constraints naturally lead us to consider a variable vaccination rate
$\phi(S(t))$, instead of vaccinating a fixed proportion $\phi > 0$ per
time step.

There are many choices for the function $\phi(S(t))$. We choose a
sigmoidal function because these curves are increasing and asymptotic.
The *Michaelis-Menten* function, $f(x) = ax/(b + x)$, is one such curve
that passes through the origin, which is convenient for our case because
when we have zero susceptibles our vaccination rate should be zero
([Bolker 2008](#ref-bolker2008)). The slope through the origin for
$f(x)$ is $a/b$ and $f(x)$ approaches $a$ as $x \rightarrow \infty$
([Bolker 2008](#ref-bolker2008)).

We interpret $a$ as the maximum number of vaccinations that can be
administered per time step, and we set $b=a$ to fix the vaccination rate
to be at most 1.

``` r
# asymptote
a = 1000
# force slope to be one at the origin
b = a
```

Suppose we can vaccinate at most 1000 individuals per time step, then
the Michaelis-Menten function will approach this asymptote with
increasing $S(t)$. The choice of $b$ restricts the slope of the curve to
be below the line $y=x$

![](./figures/Michaelis-Menten_fn-1.png)<!-- -->

To convert the Michaelis-Menten curve above to a rate, we divide by
$S(t)$.

![](./figures/var_vax-1.png)<!-- -->

# Dynamics

$$
\begin{align*}
\frac{dS}{dt} &= -\beta_{S} S\frac{I}{N_{\text{mix}}} - \phi(S) S + \rho V\\
\frac{dV}{dt} &=  \phi(S) S - \rho V - \beta_{V} V\frac{I}{N_{\text{mix}}} \\
\frac{dE}{dt} &= (\beta_{S} S + \beta_{V} V) \frac{I}{N_{\text{mix}}} - \alpha E \\
\frac{dI}{dt} &= \alpha E - \gamma_I I - \sigma I\\
\frac{dH}{dt} &= \sigma I- \gamma_H H \\
\frac{dR}{dt} &= \gamma_I I + \gamma_H H
\end{align*}
$$

This model could be optionally parameterized with $\beta_{S} = \beta$,
and $\beta_{V} = p \beta$ with $p \in (0, 1)$ to explicitly show that
the rate at which vaccinated individuals acquire infection and transmit
the disease is reduced when compared to unvaccinated individuals
([Brauer and Castillo-Chavez 2012](#ref-brauer_2012)).

# Calibration Example

The general goal of this example is to see if we can fit the SHIVER
model to COVID19 hospitilization data to estimate plausible transmission
rates for vaccinated and unvaccinated individuals.

We first load the model specification from the model library.

``` r
spec = mp_tmb_library("starter_models","shiver", package="macpan2")
```

## Calibration Scenario

Let’s create a scenario for calibration. Suppose we have 3 months of
daily COVID19 hospitalization data where some daily reports are missing.

``` r
expected_daily_reports = 90 # days
missed_reports = 10
actual_daily_reports = expected_daily_reports - missed_reports
```

Daily COVID19 hospitalization data for Ontario was obtained from the
[Ontario Data
Catalogue](https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/274b819c-5d69-4539-a4db-f2950794138c).
The data contains daily hospitalization counts stratitfied by
vaccination status and severity of hospitalization care. For simplicity,
we aggregate all counts into one. We will use the first data point
(August 10, 2021) as the initial condition for H, and the second data
point (August 11, 2021) as day 1 of the scenario. To incorporate missed
data reporting, we randomly remove 10 records.

``` r
set.seed(expected_daily_reports)
# Obtained from here:
# https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/274b819c-5d69-4539-a4db-f2950794138c
daily_hospitalizations = (read.csv(
  system.file(
      "starter_models"
    , "shiver"
    , "data"
    , "hospitalizations_ontario.csv"
    , package = "macpan2"
  )
  , row.names = NULL
  ) 
  |> rename(time = X_id)
  |> group_by(time)
  # assuming all hospital admissions (ICU or not) are part of H compartment
  |> summarize(value = sum(icu_unvac, icu_partial_vac, icu_full_vac,
                            hospitalnonicu_unvac, hospitalnonicu_partial_vac,
                            hospitalnonicu_full_vac))
  |> ungroup()
)

head(daily_hospitalizations)
#> # A tibble: 6 × 2
#>    time value
#>   <int> <int>
#> 1     1    63
#> 2     2    93
#> 3     3   112
#> 4     4   139
#> 5     5   142
#> 6     6   127

reported_hospitalizations = (daily_hospitalizations
  # remove first record (Aug 10, this will be the initial condition for H)
  |> filter(time!=1) 
  # update all times to set Aug 11 to be day 1 of the scenario
  |> mutate(time=time-1) 
  |> head(expected_daily_reports)
  |> mutate(matrix="H")
  |> sample_n(actual_daily_reports)
)
```

## Deciding on Defaults

We want to check the model specification defaults to see what needs
updating.

``` r
# We can view model spec default values here to see if we need to make any
# changes
mp_default(spec)
#>     matrix row col value
#> 1        a         1e+01
#> 2        b         1e+01
#> 3      rho         5e-02
#> 4   beta_s         2e-01
#> 5   beta_v         5e-02
#> 6    alpha         5e-01
#> 7  gamma_i         1e-01
#> 8  gamma_h         7e-02
#> 9    sigma         5e-02
#> 10       N         1e+02
#> 11       I         1e+00
#> 12       V         0e+00
#> 13       E         0e+00
#> 14       H         0e+00
#> 15       R         0e+00
```

We need to update the population size $N$ to the population of Ontario
at the time. [Statistics
Canada](https://doi.org/10.25318/1710000901-eng) estimates a population
of approximately 14.8 million in the second quarter of 2021.

``` r
# N = population size
# ---------------------
N = 14.8e7
```

Vaccination data from [Public Health
Ontario](https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool)
can help us get an estimate for the maximum number of vaccines that can
be administered per day. We average weekly vaccination counts for the
month preceding the scenario, and use this as our estimate for the
Michaelis-Menten asymptote parameter $a$.

``` r
# phi = vaccination rate
# ---------------------
# for the month of July 2021, number of individuals vaccinated per week in ontario
july_vax = c(
    135177
  , 124468
  , 111299
  , 100825
)
## average july weekly data to per day (4 weeks = 28 days)
# use this as the maximum number of daily vaccinations
a = sum(july_vax)/28 # seems plausible
```

We assume the average protection acquired from vaccination lasts 6
months (180 days), individuals spend on average 3.3 days in the exposed
compartment, and 10% of infectious individuals on average require
hospital care before recovery.

``` r
# rho = waning vaccination
# ---------------------
rho = 1/180 # average protection lasts 180 days 

# alpha = exposure rate
# ---------------------
alpha = 1/3.3 #3.3 days in exposed class 

# sigma = hospitalization rate
# ---------------------
# 10% of infections result in hospitalizations
sigma = 1/10
```

The default recovery rates mean an individual takes 10 days on average
to move from I to R, and 14 days on average to go from H to R. These
rates seem plausible for this scenario. Note that the recovery class in
this model, also called the removed compartment, includes deaths from
the hospitalization class. We could speculate that the recovery rate
from H to R could be shorter than the recovery rate from I to R if
hospitalized individuals are more likely to die for instance than
recover (and given death on average happens quicker than recovery from
infection). For now, let’s stick with the 2 week recovery rate for H,
given we assumed in the observed data above that all hospitalized
individuals are in H regardless of severity.

We want to estimate the transmission parameters `beta_s` and `beta_v`.
It’s more challenging to interpret these state dependent parameters, but
we would expect transmission involving vaccinated suceptibles would be
less effective than unvaccinated transmission (`beta_v` \< `beta_s`).
These defaults meet this minimal assumption. Additionally,
$R_{0,S} \approx \beta_s/\gamma_i = 2$ which seems reasonable.

We want to update the initial states with data we can find. [Public
Health
Ontario](https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool)
provides us with daily estimates for $V(0)$ and $I(0)$ and $H(0)$. There
is no data on initial exposure, however we know it is improbable that
there are no exposed individuals initially. We will estimate this value
in addition to transmission parameters when calibrating. Finally, since
we are not interested in the dynamics of S or R, it makes sense to leave
the default $R = 0$, even though we have prior knowledge that by the
summer of 2021 $R > 0$. Individuals in $R$ are removed from the
transmission dynamics, so initializing $R$ to a non-zero value will only
deplete the initial susceptible population by this value.

``` r
## Initial Conditions
# ---------------------

# V0 = initial V
# ---------------------
# We can get weekly Ontario vaccination data here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# If we divide the number of individuals vaccinated in the first week of August 2021 by 7
V0 = 71096/7


# I0 = initial I
# ---------------------
# We can also look at the number of weekly cases of COVID for this time period from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# and divide by 7 as above to make it a daily estimate.
I0 = 1903/7

# H0 = initial H
# ---------------------
# Use the first observed data point (Aug 10, 2021) as initial H.
H0 = daily_hospitalizations |> filter(row_number()==1) |> select(value) |> pull()
```

## Simulating Dynamics

Before optimizing, we want to make sure the dynamics look reasonable, so
we simulate from the calibrator object using `mp_trajectory`. The
trajectories look mostly as expected. The sharp initial increase in $E$
and decrease in $I$ might be attributed to setting $E(0)=1$

``` r

# state variables
states = c("S","H","I","V","E","R")

# set up calibrator
shiver_calibrator = mp_tmb_calibrator(
    spec = spec
  , data = reported_hospitalizations
  , traj = "H"
  # parameters we want to estimate (transmission rates)
  # we also want to estimate initial E
  , par = c("beta_v","beta_s","E") 
  , outputs = states
  # update defaults
  , default = list(
      N=N
    , V=V0
    , I=I0
    , H=H0
    , a = a
    , b = a
    , alpha=alpha
    , sigma=sigma
    , rho = rho
  ) 
)
# print to check
shiver_calibrator
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - V - E - I - H - R
#> 2: N ~ sum(S, V, E, I, H, R)
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#>  1: N_mix ~ N - H
#>  2: vaccination ~ S * ((a * S)/(b + S))/S
#>  3: unvaccinated_exposure ~ S * I * beta_s/N_mix
#>  4: vaccine_waning ~ V * rho
#>  5: vaccinated_exposure ~ V * I * beta_v/N_mix
#>  6: infection ~ E * alpha
#>  7: infectious_recovery ~ I * gamma_i
#>  8: hospitalizations ~ I * sigma
#>  9: hospital_recovery ~ H * gamma_h
#> 10: S ~ S - vaccination + vaccine_waning - unvaccinated_exposure
#> 11: V ~ V + vaccination - vaccine_waning - vaccinated_exposure
#> 12: E ~ E + unvaccinated_exposure + vaccinated_exposure - infection
#> 13: I ~ I + infection - infectious_recovery - hospitalizations
#> 14: R ~ R + infectious_recovery + hospital_recovery
#> 15: H ~ H + hospitalizations - hospital_recovery
#> 
#> ---------------------
#> After the simulation loop (t = T + 1):
#> ---------------------
#> 1: sim_H ~ rbind_time(H, obs_times_H)
#> 
#> ---------------------
#> Objective function:
#> ---------------------
#> ~-sum(dpois(obs_H, clamp(sim_H)))

# trajectory has 90 time steps (which is what we expect)
nrow(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> select(time) 
  |> unique()
)
#> Constructing atomic D_lgamma
#> [1] 90
  

# which time steps are missing in observed data
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> anti_join(reported_hospitalizations, by="time")

) 
#>    matrix time row col    value
#> 1       H    6   0   0 139.1204
#> 2       H   26   0   0 211.5700
#> 3       H   32   0   0 219.1765
#> 4       H   38   0   0 223.9575
#> 5       H   41   0   0 225.6073
#> 6       H   48   0   0 228.1288
#> 7       H   57   0   0 229.5688
#> 8       H   61   0   0 229.7930
#> 9       H   70   0   0 229.7078
#> 10      H   73   0   0 229.5471

# before optimizing, do the dynamics look reasonable? 
(shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales='free')
    + geom_line()
    + theme_bw()
)
```

![](./figures/simulating_dynamics-1.png)<!-- -->

## Estimating Parameters

We are now ready for the optimization step.

``` r
# optimize to estimate parameters
# this converges!
mp_optimize(shiver_calibrator)
#> outer mgc:  42643.39 
#> Constructing atomic D_lgamma
#> outer mgc:  2089.518 
#> outer mgc:  3119.486 
#> outer mgc:  19503.69 
#> outer mgc:  151.7291 
#> outer mgc:  173.1312 
#> outer mgc:  0.1312277 
#> outer mgc:  4.219828e-06
#> $par
#>      params      params      params 
#>   0.1223135   0.1890243 148.1143104 
#> 
#> $objective
#> [1] 388.2653
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 7
#> 
#> $evaluations
#> function gradient 
#>       12        8 
#> 
#> $message
#> [1] "relative convergence (4)"

# look at estimates with CI
est_coef = mp_tmb_coef(shiver_calibrator, conf.int=TRUE)
#> outer mgc:  4.219828e-06 
#> outer mgc:  35.7526 
#> outer mgc:  35.74831 
#> outer mgc:  12261.11 
#> outer mgc:  11884.68 
#> outer mgc:  0.9761616 
#> outer mgc:  0.9761703 
#> outer mgc:  94055.89
est_coef
#>       term    mat row col default  type    estimate    std.error    conf.low
#> 1   params beta_v   0   0    0.05 fixed   0.1223135  0.716183274  -1.2813799
#> 2 params.1 beta_s   0   0    0.20 fixed   0.1890243  0.002902899   0.1833347
#> 3 params.2      E   0   0    0.00 fixed 148.1143104 10.932211913 126.6875687
#>     conf.high
#> 1   1.5260069
#> 2   0.1947139
#> 3 169.5410520
```

We get a realistic estimate for `beta_s` at 0.19 with a small standard
error and the estimated initial number of exposed individuals, 148,
seems plausible with a standard error of 11. The estimate for $\beta_v$
however has a large standard error and the confidence interval contains
zero indicating we are not learning about this parameter.

To check the fit we plot the observed data as well as the trajectories
of all states.

    #> outer mgc:  4.219828e-06 
    #> outer mgc:  35.7526 
    #> outer mgc:  35.74831 
    #> outer mgc:  12261.11 
    #> outer mgc:  11884.68 
    #> outer mgc:  0.9761616 
    #> outer mgc:  0.9761703 
    #> outer mgc:  94055.89

![](./figures/fit-1.png)<!-- -->![](./figures/fit-2.png)<!-- -->

The simulated hospitalization trajectory fits the data well, and the
other trajectories look as expected.

## Re-parameterizing and Introducing Transformations

For better interpretability we can re-parameterize the model with one
transmission rate, `beta`, and a proportion, `p` in (0,1), representing
the reduced transmission rate for vaccinated people. We also wish to
parameterize `{I0, E0}` to `{I0, E0/I0}` to de-correlate `I0` and `E0`.

We use the log transformation for `beta` and `E0/I0` because both
quantities take on only positive values. We use the logistic
transformation for the proportion `p` to constrain it to the domain
(0,1).

We define a new model specification object with these changes, and set
the default values for all three parameters to be small values (0.01)
because they all have a lower bound of 0.

``` r
# Create a new model specification with these changes:
#
# - update the before step to transform "new" parameters
reparameterized_spec = mp_tmb_insert(spec
     , phase = "before"
     , at=1L
     , expressions = list(
         E ~ exp(log_E_I_ratio) * I
       , beta ~ exp(log_beta)
       , p ~ 1/(1+exp(-logit_p))
     )
     , default = list(
         logit_p = qlogis(1e-2)
       , log_beta = log(1e-2)
       , log_E_I_ratio = log(1e-2) 
     )
)

# - overwrite existing exposure terms with new ones
reparameterized_spec = mp_tmb_update(reparameterized_spec
    , phase = "during"
     # exposure expressions start at step 4 in the during phase
    , at=4L
    , expressions = list(
        mp_per_capita_flow("S", "E", unvaccinated_exposure ~ I * beta/N_mix)
      , mp_per_capita_flow("V", "E", vaccinated_exposure ~  I * beta * p/N_mix)
    )
)


# all changes have been made
print(reparameterized_spec)
#> ---------------------
#> Default values:
#> ---------------------
#>         matrix row col     value
#>              a          10.00000
#>              b          10.00000
#>            rho           0.05000
#>         beta_s           0.20000
#>         beta_v           0.05000
#>          alpha           0.50000
#>        gamma_i           0.10000
#>        gamma_h           0.07000
#>          sigma           0.05000
#>              N         100.00000
#>              I           1.00000
#>              V           0.00000
#>              E           0.00000
#>              H           0.00000
#>              R           0.00000
#>        logit_p          -4.59512
#>       log_beta          -4.60517
#>  log_E_I_ratio          -4.60517
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: E ~ exp(log_E_I_ratio) * I
#> 2: beta ~ exp(log_beta)
#> 3: p ~ 1/(1 + exp(-logit_p))
#> 4: S ~ N - V - E - I - H - R
#> 5: N ~ sum(S, V, E, I, H, R)
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: N_mix ~ N - H
#> 2: mp_per_capita_flow(from = "S", to = "V", rate = vaccination ~ 
#>      ((a * S)/(b + S))/S)
#> 3: mp_per_capita_flow(from = "V", to = "S", rate = vaccine_waning ~ 
#>      rho)
#> 4: mp_per_capita_flow(from = "S", to = "E", rate = unvaccinated_exposure ~ 
#>      I * beta/N_mix)
#> 5: mp_per_capita_flow(from = "V", to = "E", rate = vaccinated_exposure ~ 
#>      I * beta * p/N_mix)
#> 6: mp_per_capita_flow(from = "E", to = "I", rate = infection ~ alpha)
#> 7: mp_per_capita_flow(from = "I", to = "R", rate = infectious_recovery ~ 
#>      gamma_i)
#> 8: mp_per_capita_flow(from = "I", to = "H", rate = hospitalizations ~ 
#>      sigma)
#> 9: mp_per_capita_flow(from = "H", to = "R", rate = hospital_recovery ~ 
#>      gamma_h)
```

Next we calibrate and specify the transformed parameters to estimate.

``` r
# let's calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = reparameterized_spec
  , data = reported_hospitalizations
  , traj = "H"
  # now we want to estimate the transformed parameters
  , par = c("log_beta","logit_p","log_E_I_ratio")
  , outputs = states
  , default = list(
      N=N
    , V=V0
    , I=I0
    , H=H0
    , a = a
    , b = a
    , alpha=alpha
    , sigma=sigma
    , rho = rho
  ) 
)


# optimize to estimate transmission parameters
# converges with warnings
mp_optimize(shiver_calibrator)
#> outer mgc:  1363.401 
#> outer mgc:  3937.472
#> Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
#> NA/NaN function evaluation
#> outer mgc:  49082.3
#> Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
#> NA/NaN function evaluation
#> outer mgc:  67016.01 
#> outer mgc:  15308.21 
#> outer mgc:  1531.882 
#> outer mgc:  30.05599 
#> outer mgc:  4863.127 
#> outer mgc:  893.994 
#> outer mgc:  866.9924 
#> outer mgc:  4723.983 
#> outer mgc:  239.2728 
#> outer mgc:  5.803802 
#> outer mgc:  0.009437616 
#> outer mgc:  3.288246 
#> outer mgc:  31.30304 
#> outer mgc:  24.87539 
#> outer mgc:  39.42675 
#> outer mgc:  11.40318 
#> outer mgc:  16.84259 
#> outer mgc:  8.625365 
#> outer mgc:  1.555336 
#> outer mgc:  0.3827783 
#> outer mgc:  0.001030864 
#> outer mgc:  4.004449e-07
#> $par
#>     params     params     params 
#> -1.6658798  0.6062206 -0.6072924 
#> 
#> $objective
#> [1] 388.2653
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 25
#> 
#> $evaluations
#> function gradient 
#>       39       25 
#> 
#> $message
#> [1] "both X-convergence and relative convergence (5)"
```

    #> outer mgc:  4.004449e-07 
    #> outer mgc:  434.8874 
    #> outer mgc:  431.0298 
    #> outer mgc:  0.2922581 
    #> outer mgc:  0.2923429 
    #> outer mgc:  27.38987 
    #> outer mgc:  27.36335 
    #> outer mgc:  17806.78
    #>       term       mat row col default  type  estimate   std.error     conf.low
    #> 1   params      beta   0   0    0.01 fixed 0.1890243 0.002901057 1.834230e-01
    #> 2 params.2 E_I_ratio   0   0    0.01 fixed 0.5448241 0.040194207 4.714755e-01
    #> 3 params.1         p   0   0    0.01 fixed 0.6470782 3.795780771 2.220446e-16
    #>   conf.high
    #> 1 0.1947966
    #> 2 0.6295836
    #> 3 1.0000000

The estimates for our parameters are close to the the previous
estimates, which makes sense because we only re-parameterized instead of
making changes to the model specification. The ratio of initial exposed
to initial number of infected is 0.54. Given we specified the initial
$I$ as 272, the estimated initial number of exposed is approximately
148. A reduction in transmission by 0.65 percent could be plausible but
the confidence interval is effectively all possible values for `p`.

## Runge-Kutta 4

By default, `macpan2` uses the first order Euler method for solving
ODEs. We might be able to improve our estimates if we use a higher order
method such as Runge-Kutta 4. Provided our model is specified with the
appropriate flow rates between compartments (it is, see
`?mp_per_capita_flow`), we can easily perform Runge-Kutta 4 with
`mp_rk4()`.

``` r
# let's calibrate
shiver_calibrator_rk4 = mp_tmb_calibrator(
    spec = reparameterized_spec |> mp_rk4()
  , data = reported_hospitalizations
  , traj = "H"
  , par = c("log_beta","logit_p","log_E_I_ratio")
  , outputs = states
  # defaults are specified in model spec alreadt
)

# optimize
# converges with warning
mp_optimize(shiver_calibrator_rk4)
#> outer mgc:  1103.724
#> Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
#> NA/NaN function evaluation
#> outer mgc:  37098.44
#> Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
#> NA/NaN function evaluation
#> outer mgc:  39502.02 
#> outer mgc:  3149.248 
#> outer mgc:  4868.253 
#> outer mgc:  658.4232 
#> outer mgc:  605.0627 
#> outer mgc:  641.1094 
#> outer mgc:  17.00907 
#> outer mgc:  466.3379 
#> outer mgc:  114.0384 
#> outer mgc:  278.1696 
#> outer mgc:  429.5699 
#> outer mgc:  623.1688 
#> outer mgc:  347.6196 
#> outer mgc:  197.0257 
#> outer mgc:  99.52554 
#> outer mgc:  55.6952 
#> outer mgc:  31.17969 
#> outer mgc:  17.40414 
#> outer mgc:  24.18022 
#> outer mgc:  8.987731 
#> outer mgc:  3.49001 
#> outer mgc:  1.310126 
#> outer mgc:  0.485819 
#> outer mgc:  0.1792602 
#> outer mgc:  0.06601972 
#> outer mgc:  0.0242973 
#> outer mgc:  0.008939834 
#> outer mgc:  0.003288965 
#> outer mgc:  0.001209967 
#> outer mgc:  0.0004451255 
#> outer mgc:  0.000163753 
#> outer mgc:  6.024142e-05
#> $par
#>    params    params    params 
#> -1.051587 19.798125  1.084825 
#> 
#> $objective
#> [1] 65430.73
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 33
#> 
#> $evaluations
#> function gradient 
#>       44       34 
#> 
#> $message
#> [1] "relative convergence (4)"

# looking at coefficients and CIs
rk4_coef <- (mp_tmb_coef(shiver_calibrator_rk4, conf.int=TRUE)
       |> backtrans()
)
#> outer mgc:  6.024142e-05 
#> outer mgc:  30.47302 
#> outer mgc:  30.60307 
#> outer mgc:  6.018122e-05 
#> outer mgc:  6.03017e-05 
#> outer mgc:  3.146482 
#> outer mgc:  3.147977 
#> outer mgc:  91.19851

rk4_coef
#>       term       mat row col default  type  estimate    std.error     conf.low
#> 1   params      beta   0   0    0.01 fixed 0.3493828 2.314450e-03 3.448758e-01
#> 2 params.2 E_I_ratio   0   0    0.01 fixed 2.9589227 9.581253e-02 2.776969e+00
#> 3 params.1         p   0   0    0.01 fixed 1.0000000 1.886263e-06 2.220446e-16
#>   conf.high
#> 1 0.3539486
#> 2 3.1527990
#> 3 1.0000000
# rk4 doesn't help us learn more about p
# let's try adding more data
```

In this scenario, Runge-Kutta 4 did not improve our estimate for `p`.

## Fitting to Multiple Trajectories

If we include more observed data, can we get an estimate for the
proportion `p`?

We obtain COVID case count data from the [Ontario Data
Catalogue](https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/eed63cf2-83dd-4598-b337-b288c0a89a16)
for an experiment to fit the SHIVER model to both hospitalization and
incidence data.

``` r
# calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = reparameterized_spec |> mp_rk4()
    # row bind both observed data
  , data = rbind(reported_hospitalizations, reported_cases)
    # fit both trajectories
  , traj = c("H","infection")
  , par = c("log_beta","logit_p","log_E_I_ratio")
  , outputs=c(states, "infection")
)
```

Next we optimize, and look at our estimates.

    #> outer mgc:  0.003348001 
    #> outer mgc:  650.4109 
    #> outer mgc:  651.2872 
    #> outer mgc:  0.003338541 
    #> outer mgc:  0.003357471 
    #> outer mgc:  26.66361 
    #> outer mgc:  26.65704 
    #> outer mgc:  122.8709
    #>       term       mat row col default  type  estimate    std.error     conf.low
    #> 1   params      beta   0   0    0.01 fixed 0.2771663 4.530111e-04 2.762798e-01
    #> 2 params.2 E_I_ratio   0   0    0.01 fixed 0.6578376 1.710869e-02 6.251455e-01
    #> 3 params.1         p   0   0    0.01 fixed 1.0000000 3.381665e-06 2.220446e-16
    #>   conf.high
    #> 1 0.2780556
    #> 2 0.6922394
    #> 3 1.0000000

We were not able to estimate `p` again, note the confidence interval is
again the entire domain of possible values for `p`.

The fitted curves, in red, are far from following the observed data
points. We can clearly see adding more noisy data leads to major fitting
problems.

    #> outer mgc:  0.003348001 
    #> outer mgc:  650.4109 
    #> outer mgc:  651.2872 
    #> outer mgc:  0.003338541 
    #> outer mgc:  0.003357471 
    #> outer mgc:  26.66361 
    #> outer mgc:  26.65704 
    #> outer mgc:  122.8709

![](./figures/mult_traj_fit-1.png)<!-- -->

## Parameter Identifiability

As a first step in calibration, we advise specifying a default for the
parameter of interest, simulate data from the model and then calibrate
to this simulated data to see if the default parameter can be recovered.
If we are unable to recover the default, this could mean there are
identifiability issues with the model and parameter. This should have
been the first step in this example, before calibrating to observed
data.

Here we specify a ground truth for `p` and `beta`, and simulate data
from the model using these default values. We perform two calibrations,
first we estimate `p` with a fixed `beta`, and then we do the reverse.

``` r
# set true values
true_p = 0.2
true_beta = 0.3 
```

``` r
# simulate fake data
simulated_data = (reparameterized_spec
  |> mp_simulator(
      time_steps = expected_daily_reports
    , outputs=states
    , default=list(
        logit_p=qlogis(true_p)
      , log_beta=log(true_beta)
    )
    )
  |> mp_trajectory()
  # add some noise
  |> mutate(value = rpois(n(),value))
  |> select(-c(row,col))
)


## fix beta estimate p
fixed_beta = mp_tmb_calibrator(
    spec = reparameterized_spec |> mp_rk4()
  , data = simulated_data
  , traj = states
  , par = c("logit_p")
  , outputs=states
)
# converges, but not getting estimate for `p`
mp_optimize(fixed_beta)
#> outer mgc:  0.3095517 
#> outer mgc:  7.932559e-23
#> $par
#>  params 
#> 53.5867 
#> 
#> $objective
#> [1] 1146.772
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 2
#> 
#> $evaluations
#> function gradient 
#>        6        2 
#> 
#> $message
#> [1] "relative convergence (4)"
(mp_tmb_coef(fixed_beta, conf.int=TRUE)
  |> backtrans()
)
#> outer mgc:  7.932559e-23 
#> outer mgc:  7.924631e-23 
#> outer mgc:  7.940496e-23 
#> outer mgc:  1.756503e-25
#>     term mat row col default  type estimate    std.error     conf.low conf.high
#> 1 params   p   0   0    0.01 fixed        1 2.493065e-05 2.220446e-16         1
## fix p estimate beta
fixed_a = mp_tmb_calibrator(
  spec = reparameterized_spec |> mp_rk4()
  , data = simulated_data
  , traj = states
  , par = c("log_beta")
  , outputs=states
)
# converges and recovering true beta
mp_optimize(fixed_a)
#> outer mgc:  18.62197
#> Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
#> NA/NaN function evaluation
#> outer mgc:  6.218476 
#> outer mgc:  0.08276421 
#> outer mgc:  1.524823e-05 
#> outer mgc:  3.494608e-13
#> $par
#>     params 
#> -0.9757211 
#> 
#> $objective
#> [1] 855.045
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 5
#> 
#> $evaluations
#> function gradient 
#>        8        5 
#> 
#> $message
#> [1] "both X-convergence and relative convergence (5)"
(mp_tmb_coef(fixed_a, conf.int=TRUE)
  |> backtrans()
)
#> outer mgc:  3.494608e-13 
#> outer mgc:  0.8888749 
#> outer mgc:  0.8853693 
#> outer mgc:  6.93426
#>     term  mat row col default  type  estimate  std.error  conf.low conf.high
#> 1 params beta   0   0    0.01 fixed 0.3769204 0.01265488 0.3529158 0.4025578
```

We are able to recover the transmission rate `beta` but not the
proportion `p`, suggesting model identifiability issues with this
parameter.

# Model Specification

This model has been specified in the `shiver` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/shiver/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details).

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bolker2008" class="csl-entry">

Bolker, Benjamin M. 2008. *Ecological Models and Data in R*. Princeton:
Princeton University Press. <https://doi.org/doi:10.1515/9781400840908>.

</div>

<div id="ref-brauer_2012" class="csl-entry">

Brauer, Fred, and Carlos Castillo-Chavez. 2012. *Mathematical Models in
Population Biology and Epidemiology*. Vol. 40. Texts in Applied
Mathematics. New York, NY: Springer New York.
<https://doi.org/10.1007/978-1-4614-1686-9>.

</div>

<div id="ref-wiedermann_2016" class="csl-entry">

Wiedermann, Ursula, Erika Garner-Spitzer, and Angelika Wagner. 2016.
“Primary Vaccine Failure to Routine Vaccines: Why and What to Do?”
*Human Vaccines & Immunotherapeutics* 12 (1): 239–43.
<https://doi.org/10.1080/21645515.2015.1093263>.

</div>

</div>
