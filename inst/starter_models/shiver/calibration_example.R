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

spec = mp_tmb_library("starter_models","shiver", package="macpan2")
 
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
spec$default

# default phi=1/10, means individuals spend 10 days in vaccination compartment
# Let's change this to 1/14, meaning on average vaccinated individuals develop
# an immune response in two weeks.
phi = 1/14

# On average individuals spend 3.3 days in exposed class 
# (estimate taken from the default value for sigma in macpan_base)
alpha = 1/3.3

# Hospital stays are on average 10 days
# (estimate taken from the default value for rho in macpan_base)
sigma = 1/10 

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


# We want to estimate the transmission parameters beta_v and beta_s. It's more
# challenging to interpret these state dependent parameters, but we would 
# expect transmission involving vaccinated suceptibles would be less effective than 
# unvaccinated transmission (beta_v < beta_s). These defaults meet this minimal assumption.


## Initial Conditions

# We need to update N to the population of Ontario (at the time)
# From StatsCan here: https://tinyurl.com/2cdfa52n, second quarter of 2021
N = 14.8e7

# We can get weekly Ontario vaccination data here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# If we divide the number of individuals vaccinated in the first week of August 2021 by 7, we
# can make this a daily estimate on the initial numbers in the vaccination class.
V0 = 71096/7

# We can also look at the number of weekly cases of COVID for this time period from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# and divide by 7 as above to make it a daily estimate.
I0 = 1903/7

# Use the first observed data point (Aug 10, 2021) as initial H
H0 = daily_hospitalizations |> filter(row_number()==1) |> select(value) |> pull()

# We don't have data on initial exposure, however we know it is improbable that there
# are no exposed individuals initially. For now, setting to 1, since we don't have a better
# estimate to put in its place.
E0 = 1

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
  , par = c("beta_v","beta_s") 
  , outputs = states
  , default = list(N=N, V=V0, I=I0, H=H0, E=E0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
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
# yes
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
# this seems plausible beta_v ~ 0.1 < beta_s ~ 0.3
# CIs aren't too large
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

# Create a new model specification with these changes:
#
# - update the before step to transform "new" parameters,
# - set defaults to small values because they both have a lower
# bound of zero
updated_spec = mp_tmb_insert(spec
     , phase = "before"
     , at=1L
     , expressions = list(
         beta ~ exp(log_beta)
       , a ~ 1/(1+exp(-logit_a))
     )
     , default = list(
        logit_a = qlogis(1e-2)
     , log_beta = log(1e-2)
     )
)

# - overwrite existing exposure terms with new ones
updated_spec = mp_tmb_update(updated_spec
    , phase = "during"
     # exposure expressions start at step 4 in the during phase
    , at=4L
    , expressions = list(
         unvaccinated_exposure ~ S * I * beta/N_mix
       , vaccinated_exposure ~ V * I * beta * a/N_mix
    )
)

# all changes have been made
print(updated_spec)

# let's calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = updated_spec
  , data = reported_hospitalizations
  , traj = "H"
  # now we want to estimate the transformed parameters
  , par = c("log_beta","logit_a")
  , outputs = states
  , default = list(N=N, V=V0, I=I0, H=H0, E=E0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
  )
)


# optimize to estimate transmission parameters
# converges, with warning
mp_optimize(shiver_calibrator)

# looking at coefficients and CIs
# we need to back transform to interpret
# beta is approximately 0.3 (as in beta_s estimate previously)
cc <- (mp_tmb_coef(shiver_calibrator, conf.int=TRUE)
       |> backtrans()
)
cc |> filter(mat == "beta")

# `a` is approximately 0.35, with a CI of (0.31,0.38)
# We interpret `a` as a 35% reduction in transmission 
# for vaccinated suceptibles.
# 0.35*beta ~ 0.1 = the estimate we obtained for beta_v previously
cc |> filter(mat == "a")

# how does fit the look with these parameters
# identical to previous fit (because all we did was re-parameterize)
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



## -------------------------
## fitting to multiple trajectories
## -------------------------

# If we include more observed data, can we get more precise estimates?
# There is COVID case data available that we can fit to incidence.

# COVID19 case data for Ontario
# Obtained from here:
# https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/eed63cf2-83dd-4598-b337-b288c0a89a16
# The metadata for this data was not clear to me, but looking at the data I think it makes sense to
# assume these case counts are incidence (# number of new cases each day) instead of prevalence (# all active cases each day)
daily_cases = (read.csv(
    "inst/starter_models/shiver/data/cases_ontario.csv"
    # system.file(
    #     "starter_models"
    #   , "shiver"
    #   , "data"
    #   , "cases_ontario.csv"
    #   , package = "macpan2"
    # )
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


# calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = updated_spec
    # row bind both observed data
  , data = rbind(reported_hospitalizations, reported_cases)
    # fit both trajectories
  , traj = c("H","infection")
  , par = c("log_beta","logit_a")
  , outputs=c(states, "infection")
  , default = list(N=N, V=V0, I=I0_new, H=H0, E=E0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
  )
)

# optimize to estimate transmission parameters
# converges, with a warning
mp_optimize(shiver_calibrator)


# beta ~ 0.48 with reasonable CIs
cc <- (mp_tmb_coef(shiver_calibrator, conf.int=TRUE)
    |> backtrans()
)
cc |> filter(mat == "beta")

# The estimate for `a` is very small, effectively 0. This is a large change from 
# the previous estimate (35%), but could be plausible (vaccines really help lower
# transmission). However the confidence interval is very wide, essentially all possible 
# values of `a`, meaning we are not learning much about this parameter.
cc |> filter(mat == "a")

# how does data look with these parameters
# not good!
# in this case, adding more noisy data (incidence) leads to major fitting problems
# even when transmission rate estimates seem biologically possible
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




