library(macpan2)
library(ggplot2)
library(dplyr)


## -------------------------
## local function
## -------------------------

# to be included in mp_tmb_coef in the future
# see here, https://github.com/canmod/macpan2/issues/179
backtrans <- function(x) {
  vars1 <- intersect(c("default", "estimate", "conf.low", "conf.high"), names(x))
  prefix <- stringr::str_extract(x[["mat"]], "^log(it)?_")  |> tidyr::replace_na("none")
  sx <- split(x, prefix)
  for (ptype in setdiff(names(sx), "none")) {
    link <- make.link(stringr::str_remove(ptype, "_"))
    sx[[ptype]] <- (sx[[ptype]]
                    |> mutate(across(std.error, ~link$mu.eta(estimate)*.))
                    |> mutate(across(any_of(vars1), link$linkinv))
                    |> mutate(across(mat, ~stringr::str_remove(., paste0("^", ptype))))
    )
  }
  bind_rows(sx)
}


## -------------------------
## get model spec from library
## -------------------------

source("inst/starter_models/shiver/tmb.R")
#spec = mp_tmb_library("starter_models","shiver", package="macpan2")
 
## -------------------------
## calibration scenario
## -------------------------

# Use COVID19 hospitalization data to see if we can estimate
# plausible transmission rates for vaccinated and unvaccinated
# (aka susceptibles) individuals.
# This is a very simple model for COVID, and we are
# assuming a long list of things.

# Creating a scenario,
# Suppose we have 3 months of daily COVID hospitalization data
# where daily reports are missing for 10 of the days.

expected_daily_reports = 90 # days
missed_reports = 10
actual_daily_reports = expected_daily_reports - missed_reports

set.seed(expected_daily_reports)


## -------------------------
## observed data
## -------------------------

# COVID19 hospitalization data for Ontario
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


reported_hospitalizations = (daily_hospitalizations
  # remove first record (Aug 10, this will be the initial condition for H)
  |> filter(time!=1) 
  # update all times to set Aug 11 to be day 1 of the scenario
  |> mutate(time=time-1) 
  |> head(expected_daily_reports)
  |> mutate(matrix="H")
  |> sample_n(actual_daily_reports)
)

## -------------------------
## deciding on defaults
## -------------------------

# We can view model spec default values here to see if we need to make any changes.
mp_default(spec)

# N = population size
# ---------------------
# We need to update N to the population of Ontario (at the time)
# From StatsCan here: https://tinyurl.com/2cdfa52n, second quarter of 2021
N = 14.8e7

# phi = vaccination rate
# ---------------------
# Vaccination data at the time from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# for the month of July 2021, number of individuals vaccinated per week in ontario
july_vax = c(
    135177
  , 124468
  , 111299
  , 100825
)
## average july weekly data to per day (4 weeks = 28 days)
daily_vaccine_supply = sum(july_vax)/28 # seems plausible
# 0.01% of population per day
phi = daily_vaccine_supply/N

# rho = waning vaccination
# ---------------------
# current value, rho = 5e-2

## BMB: this is 5% vaccine waning *per day*, i.e. on average the protection
## acquired from vaccine only lasts 20 days.  A more realistic minimum value would
## be 1/(6 months) = 1/(180 days) ~ 0.006. Or make it 1/(200 days) = 0.005

rho = 1/180 # average protection lasts 180 days 
# this is longer than the simulation scenario, is this a problem?

# beta_v, beta_s = transmission rate
# ---------------------
# We want to estimate the transmission parameters beta_v and beta_s. It's more
# challenging to interpret these state dependent parameters, but we would 
# expect transmission involving vaccinated suceptibles would be less effective than 
## unvaccinated transmission (beta_v < beta_s). These defaults meet this minimal assumption.

## R_{0,s} approx beta_s/gamma_i = 2 which seems reasonable

# alpha = exposure rate
# ---------------------
# On average individuals spend 3.3 days in exposed class 
# (estimate taken from the default value for sigma in macpan_base)
alpha = 1/3.3
## BMB: this is large enough/the exposed period is short enough that we
## might start to worry about Euler steps (e.g. it would be worth considering
## 'hazard corrections' or Runge-Kutta)

# sigma = exposure rate
# ---------------------
# Hospital stays are on average 10 days
# (estimate taken from the default value for rho in macpan_base)
sigma = 1/10 

# gamma_i, gamma_h = recovery rates
# ---------------------
# Leaving recovery rates as is, they seem plausible:
# 1/10 ~ 0.1, takes 10 days to go from I to R
# 1/14 ~ 0.7, takes two weeks to go from H to R
#
# Note that the recovery class in this model, includes deaths from the hospitalization 
# class. We could speculate that the recovery rate from H to R could be shorter than the 
# recovery rate from I to R if hospitalized individuals are more likely to die for instance 
# than recover (and given death on average happens quicker than recovery from infection). 
# For now, let's stick with the 2 week recovery rate for H, given we assumed in the
# observed data above that all hospitalized individuals are in H (regardless of severity).

## Initial Conditions
# ---------------------

# V0 = initial V
# ---------------------
# We can get weekly Ontario vaccination data here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# If we divide the number of individuals vaccinated in the first week of August 2021 by 7, we
# can make this a daily estimate on the initial numbers in the vaccination class.
V0 = 71096/7
## BMB: I'm not sure this calculation makes sense. This is a rate, not a number or
## proportion vaxxed?

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

# E0 = initial E
# ---------------------
# We don't have data on initial exposure, however we know it is improbable that there
# are no exposed individuals initially. We will estimate this value in addition to 
# transmission parameters

# initial S, R
# ---------------------
# We are not interested in the dynamics of S or R, so it makes sense to leave the default R = 0,
# even though we have prior knowledge that by the summer of 2021 R > 0. Individuals in R are removed
# from the transmission dynamics, so initializing R to a non-zero value will only deplete the initial 
# susceptible population by this non-zero value.


## -------------------------
## calibration step
## -------------------------

# state variables
states = c("S","H","I","V","E","R")

# set up calibrator
shiver_calibrator = mp_tmb_calibrator(
    spec = spec
  , data = reported_hospitalizations
  , traj = "H"
  # parameters we want to estimate (transmission rates)
  # we also want to estimate initial E and I, because 
  # these are difficult to set in advance
  # assume we know I0 since we get an estimate from the data
  # and only estimate E
  , par = c("beta_v","beta_s","E") 
  , outputs = states
  , default = list(N=N
                   , V=V0
                   , I=I0
                   , H=H0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
                   , rho = rho
  ) 
)

shiver_calibrator

# trajectory has 90 time steps (which is what we expect)
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> select(time) 
  |> unique()
)
  

# which time steps are missing in observed data
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> anti_join(reported_hospitalizations, by="time")

) 

# before optimizing, do the dynamics look reasonable? 
# yes, although initial drop in I and jump in E
# points to the difficulty in getting the initial conditions right (comment from BMB)
# I0 was from data, E0=1 was a poor guess - which is why we are now
# estimating these values.
if (interactive()) {
  (shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales='free')
    + geom_line()
  )
}

# optimize to estimate transmission parameters
# this converges!
mp_optimize(shiver_calibrator)

# look at estimates with CI
# beta_s ~ 0.19(0.18,0.19) - good
# beta_v < beta_s but CI contains 0
# E0 ~ 148 std error 11 - good
mp_tmb_coef(shiver_calibrator, conf.int=TRUE)

# how does the fit compare with observed data?
# looks pretty good
if (interactive()) {
  (shiver_calibrator 
    |> mp_trajectory_sd(conf.int=TRUE)
    |> filter(matrix == "H")
    |> ggplot(aes(time, value))
    + geom_line(aes(y=value), colour="red")
    + geom_ribbon(aes(ymin=conf.low,ymax=conf.high), fill="red",alpha=0.3)
    + geom_point(data=reported_hospitalizations, aes(time,value))
  )
}

# does the fit create unrealistic dynamics?
# no
if (interactive()) {
  (shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales='free')
    + geom_line()
  )
}


## -------------------------
## re-parameterizing and introducing transformations
## -------------------------

# For better interpretability we can re-parameterize the model
# with one transmission rate, beta, and a proportion, `a` in (0,1),
# representing the reduced transmission rate for vaccinated
# people.

# We can use the log transformation for beta, because beta > 0.
# Since `a` in (0,1), it makes sense to use logit.

# We also wish to parameterize {I0, E0} to {I0, E0/I0} (BMB suggestion
# to de-correlate these parameters) E_I_ratio = E0/I0

# Create a new model specification with these changes:
#
# - update the before step to transform "new" parameters,
# - set defaults for transmission parameters to small values because they both have a lower
# bound of zero
# - setting E_I_ratio 
reparameterized_spec = mp_tmb_insert(spec
     , phase = "before"
     , at=1L
     , expressions = list(
         E ~ exp(log_E_I_ratio) * I
       , beta ~ exp(log_beta)
       , a ~ 1/(1+exp(-logit_a))
     )
     , default = list(
         logit_a = qlogis(1e-2)
       , log_beta = log(1e-2)
       , log_E_I_ratio = log(2) 
     )
)

# - overwrite existing exposure terms with new ones
reparameterized_spec = mp_tmb_update(reparameterized_spec
    , phase = "during"
     # exposure expressions start at step 4 in the during phase
    , at=4L
    , expressions = list(
         unvaccinated_exposure ~ S * I * beta/N_mix
       , vaccinated_exposure ~ V * I * beta * a/N_mix
    )
)


# all changes have been made
print(reparameterized_spec)

# let's calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = reparameterized_spec
  , data = reported_hospitalizations
  , traj = "H"
  # now we want to estimate the transformed parameters
  , par = c("log_beta","logit_a","log_E_I_ratio")
  , outputs = states
  , default = list(N=N
                   , V=V0
                   , I=I0
                   , H=H0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
                   , rho = rho
  ) 
)


# optimize to estimate transmission parameters
# converges with a warning
mp_optimize(shiver_calibrator)

# looking at coefficients and CIs
# we need to back transform to interpret
cc <- (mp_tmb_coef(shiver_calibrator, conf.int=TRUE)
       |> backtrans()
)
cc
# beta ~ 0.189(0.18,0.19) reasonable (similar to above estimate)
# a ~ 0.63 plausible, but CI is all values of a
# so not learning about a (same as above)
# E_I_ratio ~ 0.54(0.47,0.63) - reasonable (E0=146(127,171) similar as above estimate)

# how does fit the look with these parameters
# fit is approximately the same, which makes sense because all
# I did was re-parameterize
if (interactive()) {
  (shiver_calibrator 
   |> mp_trajectory_sd(conf.int=TRUE)
   |> filter(matrix == "H")
   |> ggplot(aes(time, value))
   + geom_line(aes(y=value), colour="red")
   + geom_ribbon(aes(ymin=conf.low,ymax=conf.high), fill="red",alpha=0.3)
   + geom_point(data=reported_hospitalizations, aes(time,value))
  )
}
# trajectories seem approximately the same as well
if (interactive()) {
  (shiver_calibrator 
   |> mp_trajectory()
   |> ggplot(aes(time, value))
   + facet_wrap(vars(matrix), scales='free')
   + geom_line()
  )
}

## -------------------------
## try Runge-Kutta 4
## -------------------------

# Can we get an estimate for `a` if we try a higher order
# method for ODE solving - rk4 instead of Euler

# set reamining defaults to create a new model specification
R0 = 0
log_E_I_ratio = log(1e-2)
logit_a = qlogis(1e-2)
log_beta = log(1e-2)
gamma_i = mp_default_list(spec)$gamma_i
gamma_h = mp_default_list(spec)$gamma_h

# create new model specification
# re-create the re-parameterized model spec
# using mp_per_capita_flow
spec_flows = mp_tmb_model_spec(
  # before is identical to previous reparameterize model
    before = list(
        E ~ exp(log_E_I_ratio) * I
      , beta ~ exp(log_beta)
      , a ~ 1/(1 + exp(-logit_a))
      , S ~ N - V - E - I - H - R
      , N ~ sum(S, V, E, I, H, R)
      )
  , during = list(
      N_mix ~ N - H
    , mp_per_capita_flow("S", "V", vaccination ~ phi)
    , mp_per_capita_flow("V", "S", vaccine_waning ~ rho)
    , mp_per_capita_flow("S", "E", unvaccinated_exposure ~ I * beta/N_mix)
    , mp_per_capita_flow("V", "E", vaccinated_exposure ~ beta * I * a/N_mix)
    , mp_per_capita_flow("E", "I", infection ~ alpha)
    , mp_per_capita_flow("I", "R", infectious_recovery ~ gamma_i)
    , mp_per_capita_flow("I", "H", hospitalizations ~ sigma)
    , mp_per_capita_flow("H", "R", hospital_recovery ~ gamma_h)
    )
  , default = list(
      N = N
    , V = V0
    , I = I0
    , H = H0
    , log_E_I_ratio = log_E_I_ratio # now E is derived, we can't estimate E0
    , phi = phi
    , alpha = alpha
    , sigma = sigma
    , rho = rho
    , logit_a = logit_a
    , log_beta = log_beta
    , gamma_i = gamma_i
    , gamma_h = gamma_h
    , R = R0
  ) 
)

# what do rk4 vs. Euler dynamics look like
shiver_rk4 = (spec_flows 
              |> mp_rk4()
              |> mp_simulator(time_steps = expected_daily_reports, outputs = states)
              |> mp_trajectory()
)
shiver_euler = (spec_flows 
              |> mp_euler()
              |> mp_simulator(time_steps = expected_daily_reports, outputs = states)
              |> mp_trajectory()
)

trajectory_comparison = list(rk4=shiver_rk4,euler=shiver_euler)|> bind_rows(.id = "update_method")

if (interactive()) {
  (trajectory_comparison
   |> ggplot(aes(time, value))
   + geom_line(aes(colour=update_method))
   + facet_wrap(vars(matrix), scales='free')
   
  )
}

# let's calibrate
shiver_calibrator_rk4 = mp_tmb_calibrator(
    spec = spec_flows |> mp_rk4()
  , data = reported_hospitalizations
  , traj = "H"
  , par = c("log_beta","logit_a","log_E_I_ratio")
  , outputs = states
  # defaults are specified in model spec (spec_flows)
)
shiver_calibrator_euler = mp_tmb_calibrator(
    spec = spec_flows |> mp_euler()
  , data = reported_hospitalizations
  , traj = "H"
  , par = c("log_beta","logit_a","log_E_I_ratio")
  , outputs = states
  # defaults are specified in model spec (spec_flows)
)

# optimize
# converges for both
mp_optimize(shiver_calibrator_rk4)
mp_optimize(shiver_calibrator_euler)

# looking at coefficients and CIs
# we need to back transform to interpret
rk4_coef <- (mp_tmb_coef(shiver_calibrator_rk4, conf.int=TRUE)
       |> backtrans()
)
euler_coef <- (mp_tmb_coef(shiver_calibrator_euler, conf.int=TRUE)
             |> backtrans()
)

rk4_coef
euler_coef # should be the same as reparameterized part above, it is
# rk4 doesn't help us learn more about a
# let's try adding more data

## -------------------------
## fitting to multiple trajectories
## -------------------------

# If we include more observed data, can we get an estimate for a?
# There is COVID case data available that we can fit to incidence.

# COVID19 case data for Ontario
# Obtained from here:
# https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/eed63cf2-83dd-4598-b337-b288c0a89a16
# The metadata for this data was not clear to me, but looking at the data I think it makes sense to
# assume these case counts are incidence (# number of new cases each day) instead of prevalence (# all active cases each day)
daily_cases = (read.csv(
    system.file(
        "starter_models"
      , "shiver"
      , "data"
      , "cases_ontario.csv"
      , package = "macpan2"
    )
  , row.names = NULL
  )
  |> rename(time = X_id)
  |> rowwise()
  # aggregate all case counts (by vax status) into one
  |> mutate(value = sum(c_across(starts_with("covid19_cases")),na.rm = TRUE))
)


reported_cases = (daily_cases
     |> filter(time!=1 & time!=2)  # remove first two records (Aug 9, Aug10 = I0)
     |> mutate(time=time-2)        # update all times to set Aug 11 to be day 1 of the scenario
     |> head(expected_daily_reports)
     |> mutate(matrix="infection") # the infection variable is incidence
     |> select(c("time","value","matrix"))
)

# use Aug 10 as initial I
# Note: this case data is from a different source than above, but I0 and I0_new
# don't differ much
I0_new = daily_cases |> filter(time==2) |> select(value) |> pull()


# check defaults again
mp_default(spec_flows)

# calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = spec_flows |> mp_rk4()
    # row bind both observed data
  , data = rbind(reported_hospitalizations, reported_cases)
    # fit both trajectories
  , traj = c("H","infection")
  , par = c("log_beta","logit_a","log_E_I_ratio")
  , outputs=c(states, "infection")
)

# optimize to estimate transmission parameters
# converges
mp_optimize(shiver_calibrator)

cc <- (mp_tmb_coef(shiver_calibrator, conf.int=TRUE)
    |> backtrans()
)
cc
# still not learning about a, and estimate for EI ratio is different
# beta not too much different from previous estimates

# how does data look with these parameters
# not good!
# in this case, adding more noisy data (incidence) leads to major fitting problems
# and does not improve estimation.
# Something also to consider, Aug-Oct doesn't capture the time frame in which we see
# usually see infection spikes for seasonal viruses (over the winter), so perhaps the 
# incidence data used here is just not informative enough about the virus dynamics
if (interactive()) {
  (shiver_calibrator 
   |> mp_trajectory_sd(conf.int=TRUE)
   |> filter(matrix %in% c("H","infection"))
   |> ggplot(aes(time, value))
   + geom_line(aes(y=value), colour="red")
   + geom_ribbon(aes(ymin=conf.low,ymax=conf.high), fill="red",alpha=0.3)
   + geom_point(data=rbind(reported_hospitalizations, reported_cases), aes(time,value))
   + facet_wrap(vars(matrix), scales='free')
  )
}


## -------------------------
## vaccine function
## -------------------------

# As a final step, can we use simulated data generated with a true
# value of `a` and calibrate to get the true value back?

# set true values
true_a = 0.2
true_beta = 0.3 
true_ratio = 0.4 #E_I_ratio

# simulate fake data
simulated_data = (spec_flows
  |> mp_simulator(
      time_steps = expected_daily_reports
    , outputs=states
    , default=list(
        logit_a=qlogis(true_a)
      , log_beta=log(true_beta)
      , log_E_I_ratio=log(true_ratio))
    )
  |> mp_trajectory()
  # add some noise
  |> mutate(value = rpois(n(),value))
  |> select(-c(row,col))
)

# calibrate with data from all states
sim_calib = mp_tmb_calibrator(
    spec = spec_flows |> mp_rk4()
  , data = simulated_data
  , traj = states
  , par = c("log_beta", "log_E_I_ratio", "logit_a")
  #, par = c("log_beta")
  , outputs=states
)

# doesn't converge
mp_optimize(sim_calib)

cc <- (mp_tmb_coef(sim_calib, conf.int=TRUE)
       |> backtrans()
)
cc

# check fit of "observed data" out of interest
# looks good
if (interactive()) {
  (sim_calib 
   |> mp_trajectory_sd(conf.int=TRUE)
   |> ggplot(aes(time, value))
   + geom_line(aes(y=value), colour="red")
   + geom_ribbon(aes(ymin=conf.low,ymax=conf.high), fill="red",alpha=0.3)
   + geom_point(data=simulated_data, aes(time,value),alpha=0.3)
   + facet_wrap(vars(matrix),scales='free')
  )
}

## last test, fix beta and estimate `a`
## then fix `a` and estimate beta

fixed_beta = mp_tmb_calibrator(
    spec = spec_flows |> mp_rk4()
  , data = simulated_data
  , traj = states
  , par = c("logit_a")
  , outputs=states
)
# converges, but not getting estimate for `a`
mp_optimize(fixed_beta)
(mp_tmb_coef(fixed_beta, conf.int=TRUE)
  |> backtrans()
)

fixed_a = mp_tmb_calibrator(
  spec = spec_flows |> mp_rk4()
  , data = simulated_data
  , traj = states
  , par = c("log_beta")
  , outputs=states
)
# converges and recovering true beta
mp_optimize(fixed_a)
(mp_tmb_coef(fixed_a, conf.int=TRUE)
  |> backtrans()
)

# Final notes:
# This points to model identifiability issues with
# estimating `a`

## -------------------------
## vaccine function
## -------------------------


## update tmb.R with this spec?


## first I want to try if I can just update
## the current spec, instead of adding a new one

print(spec)


# want to update rho which is a default

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
plot(-10:10, -erf(-10:10), type='l')
# how do we use erf for rho(t)

# use erf(x) where x(S,V,beta_v,beta_s,I,N_mix) [what about phi]

# 1: N_mix ~ N - H
# 2: vaccination ~ phi * S
# 3: vaccine_failure ~ rho * V # doesn't make sense to include this term, because we are changin rho
# 4: unvaccinated_exposure ~ S * I * beta_s/N_mix
# 5: vaccinated_exposure ~ V * I * beta_v/N_mix # i think not this term, do we want vaccine supply to depend on V ...maybe

#phi * S + S * I * beta_s/N_mix #+ V * I * beta_v/N_mix

#S * (phi + I *beta_s/N_mix)

# from spec
phi=1e-1
beta_s=2e-1
max_vax=5
N_mix=100

S=seq(100,0,by=-1)
I=seq(0,100,by=1)

unvax=S * (phi + I *beta_s/N_mix)
plot(unvax,max_vax*erf(unvax),type='l')

# how does this prevent S and V from going negative



