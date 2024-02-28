library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","shiver",package="macpan2")
spec 

## -------------------------
## calibration goal
## -------------------------

# use COVID19 hospitalization data to see if we can estimate
# plausible transmission rates for vaccinated and unvaccinated
# (aka susceptibles)
# this is a very simple model for COVID, and we are
# assuming a long list of things:


## -------------------------
## observed data
## -------------------------

# COVID19 hospitalization data for Ontario (orginally named vac_status_hosp_icu.csv)
# https://data.ontario.ca/dataset/covid-19-vaccine-data-in-ontario/resource/274b819c-5d69-4539-a4db-f2950794138c
daily_hospitalizations = (read.csv(
  "inst/starter_models/shiver/data/hospitalizations_ontario.csv"
  # system.file(
  #   "starter_models"
  #   , "shiver"
  #   , "hostpitalizations_ontario.csv"
  #   , package = "macpan2"
  # )
  , row.names = NULL
  ) 
  %>% rename(time = X_id)
  %>% group_by(time)
  # assuming all hospital 
  %>% summarize(value = sum(icu_unvac, icu_partial_vac, icu_full_vac,
                            hospitalnonicu_unvac, hospitalnonicu_partial_vac, hospitalnonicu_full_vac))
  %>% ungroup()
)

# making a scenario,
# suppose we have 3 months of daily COVID hospitalization data
# where daily reports are missing for 10 of the days
expected_daily_reports = 90 # days
missed_reports = 10
actual_daily_reports = expected_daily_reports - missed_reports

set.seed(expected_daily_reports)
reported_hospitalizations = (daily_hospitalizations
  %>% filter(time!=1) # remove first record (Aug 10, this will be H0)
  %>% head(expected_daily_reports)
  %>% mutate(row=0L,col=0L,matrix="H")
  %>% sample_n(actual_daily_reports)
)

## -------------------------
## deciding on defaults
## -------------------------

# we can view model spec default values to see if we need to make any changes
spec$default

# we need to update N
# population of Ontario (at the time)
# second quarter of 2021 - from StatsCan here: https://tinyurl.com/2cdfa52n
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901&cubeTimeFrame.startMonth=04&cubeTimeFrame.startYear=2021&cubeTimeFrame.endMonth=04&cubeTimeFrame.endYear=2021&referencePeriods=20210401%2C20210401
N = 14.8e7

# default phi 10% (proportion of susceptibles that get vaccinated) seems too big
# looking at vaccination data at the time from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# for the month of July 2021, number of individuals vaccinated per week in ontario
july_vax = c(
    135177
  , 124468
  , 111299
  , 100825
)
# average july data to per day (4 weeks = 25 days)
daily_vaccine_supply = sum(july_vax)/25 # seems plausible
phi = daily_vaccine_supply/N # 0.01%, maybe plausible for the time period

# we want to estimate the betas so we leave as is

# on average individuals spend 3.3 days in exposed class (taken from macpan_base sigma default)
alpha=1/3.3
# hospital stays are on average 10 days (taken from macpan_base rho default)
sigma=1/10 


# leaving recovery rates as is, they seem plausible
# takes 10 days from I to R
# takes 14 days from H to R

# initial conditions

# from here,
# https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# this is the number of individuals vaccinated in the first week of August 2021
# we will use this as the initial number in vax class
# make it daily, divide by 5
V0 = 71096/5

# use first data point (Aug 10, 2021) as initial H
H0 = daily_hospitalizations %>% filter(time==1) %>% select(value) %>% pull()

# also from here,https://www.publichealthontario.ca/en/Data-and-Analysis/Infectious-Disease/Respiratory-Virus-Tool
# number of cases in first week of August 2021
I0=1903/5

# we don't have data on initial exposure, so leaving E0 as is for now

# not interested in S or R, so keeping as intial conditions as is 
# start with 0 recovered individuals

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
  , par = c("beta_s", "beta_v") # we want to estimate transmission
  , outputs = states # what happens here, if you have outputs that don't include traj
  , default = list(N=N, V=V0, I=I0, H=H0
                   , phi=phi
                   , alpha=alpha
                   , sigma=sigma
                   ) 
)

# need to investigate how missing time steps work
shiver_calibrator

(shiver_calibrator 
  |> mp_trajectory()
  %>% select(time) %>% unique() #91
)
# all states
(shiver_calibrator 
  |> mp_trajectory()
  |> ggplot(aes(time, value))
  + facet_wrap(vars(matrix), scales='free')
  + geom_line()
)

# how does the fit compare with observed data
# not bad, given all simplifications?
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix == "H")
  |> ggplot(aes(time, value))
  + geom_line()
  + geom_point(data=reported_hospitalizations, aes(time,value))
)

# optimize
shiver_opt = mp_optimize(shiver_calibrator, optimizer = "nlminb")

# converges
# but beta_v is negative...
print(shiver_opt)

