print("here")

library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","shiver", package="macpan2")
 
## -------------------------
## calibration goal
## -------------------------

# Use COVID19 hospitalization data to see if we can estimate
# plausible transmission rates for vaccinated and unvaccinated
# (aka susceptibles) individuals
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
  %>% rename(time = X_id)
  %>% group_by(time)
  # assuming all hospital admissions (ICU or not) are part of H compartment
  %>% summarize(value = sum(icu_unvac, icu_partial_vac, icu_full_vac,
                            hospitalnonicu_unvac, hospitalnonicu_partial_vac,
                            hospitalnonicu_full_vac))
  %>% ungroup()
)


reported_hospitalizations = (daily_hospitalizations
  %>% filter(time!=1) # remove first record (Aug 10, this will be the initial condition H0)
  %>% mutate(time=time-1) # update all times to set Aug 11 to be day 1 of the scenario
  %>% head(expected_daily_reports)
  %>% mutate(matrix="H")
  %>% sample_n(actual_daily_reports)
)

## -------------------------
## deciding on defaults
## -------------------------

# we can view model spec default values to see if we need to make any changes
spec$default

# we need to update N to the population of Ontario (at the time)
# second quarter of 2021 - from StatsCan here: https://tinyurl.com/2cdfa52n
N = 14.8e7

# default phi 10% (proportion of susceptibles that get vaccinated) seems too big
# looking at vaccination data at the time from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
#
# for the month of July 2021, number of individuals vaccinated per week in ontario
july_vax = c(
    135177
  , 124468
  , 111299
  , 100825
)
# average july data to per day (4 weeks = 25 days) to get an estimate
# on how many vaccines can be administered daily
daily_vaccine_supply = sum(july_vax)/25 # seems plausible
# express vaccine supply as a proportion of the population
# 0.01%, maybe plausible for the time period
phi = daily_vaccine_supply/N 

# we want to estimate the betas so we leave as is
# does the starting point matter, I guess

# on average individuals spend 3.3 days in exposed class 
# (estimate taken from the default value for sigma in macpan_base)
alpha=1/3.3
# hospital stays are on average 10 days
# (estimate taken from the default value for rho in macpan_base)
sigma=1/10 


# leaving recovery rates as is, they seem plausible
# 1/10=0.1, takes 10 days to go from I to R
# 1/14 ~ 0.7, takes two weeks for hospitalized individuals to recover

# Initial Conditions

# Using the same data source to determine vaccine supply, 
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# we can get the number of individuals vaccinated in the first week of August 2021
# To use this as the inital value in V, we divide by 5 (assuming no vaccinations on the weekend?)
# to make it daily.
V0 = 71096/5

# Use the first data point (Aug 10, 2021) as initial H
H0 = daily_hospitalizations %>% filter(row_number()==1) %>% select(value) %>% pull()

# We can look at the number of cases of COVID for this time period also from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# to get the number of cases for the first week of August 2021 (divide by 5 to get daily)
I0=1903/5 

# We don't have data on initial exposure, so setting the initial
# number of exposed to half of I0?
E0=I0/2

# not interested in S or R, so keeping initial conditions as is 
# this shouldn't effect the dynamics I'm interested in (?)


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
  , par = c("beta_v","beta_s") # we want to estimate transmission
  , outputs = states
  , default = list(N=N, V=V0, I=I0, H=H0
                   , E=1
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
                   ) 
)

shiver_calibrator

# trajectory has 90 time steps (which is what we want)
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> select(time) 
  |> unique()
)
  

# which time steps are missing observed data
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix=="H")
  |> anti_join(reported_hospitalizations, by="time")

) 

# all states
if (interactive()) {
  (shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales='free')
    + geom_line()
  )
}


# optimize to estimate transmission parameters
mp_optimize(shiver_calibrator)


# converges
mp_optimizer_output(shiver_calibrator, what="all")

# look at estimates with CI
# beta_v is negative, which doesn't make sense
mp_tmb_coef(shiver_calibrator, conf.int=TRUE)

# how does the fit compare with observed data
# fit looks pretty good considering
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

# all states
# behaviour of I and E seem weird
if (interactive()) {
  (shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales='free')
    + geom_line()
  )
}


## -------------------------
## Possible next steps
## -------------------------

# * transform beta parameters, and do calibration on log scale
#   this will prevent estimates going negative at least
# * re-parameterize (as in README) to make transmission parameters
#   easier to think about
# * I can get observed data for I, and fit both trajectories (maybe
#   more data will help get reasonable estimates)
# * consider E0 more carefully
# * do S0 and R0 actually matter here?
# 
#  maybe this problem is too hard, without adding in more complexity
#   
