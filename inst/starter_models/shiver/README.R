## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.path = "./figures/"
)
system.file("utils", "round-coef.R", package = "macpan2") |> source()


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)


## ----options------------------------------------------------------------------
options(macpan2_verbose = FALSE)


## ----model_spec---------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "shiver"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 2, fig.width = 5---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_grid(spec
  , east = "(infection|progression|recovery)$"
  , north = "waning$"
  , south = "(hospitalizations|vaccination)$"
)
plot_flow_diagram(layout)


## ----Michaelis-Menten_param---------------------------------------------------
# asymptote
a = 1000
# force slope to be one at the origin
b = a


## ----Michaelis-Menten_fn, echo=FALSE------------------------------------------

# specify a sequence of S
S = seq(0,3*a,length.out=300)

# simulate Michaelis-Menten and vaccination rate for each S
varvax = simple_sims(
    iteration_exprs = list(
        mm_fn ~ (a * S)/(b + S)
      , vaccination ~ mm_fn/S
    )
  , time_steps = 1L
  , mats = list(
      mm_fn = 0
    , a = a
    , b = b
    , S = S
    , vaccination = 0
    , flow = 0
  )
)
# Michaelis-Menten function
(varvax 
   |> filter(matrix %in% c("mm_fn"))
   |> cbind(S)
   |> ggplot(aes(S, value))
   + geom_point(alpha=0.3)
   + geom_hline(yintercept = a,col="red")
   + geom_text(x=10,y=a-30,label="a", col="red")
   + geom_abline(intercept=0,slope=1,col="blue")
   + geom_text(x=a+30,y=a-125,label="y = x",col="blue")
   + theme_bw()
   + ggtitle("Michaelis-Menten function")
   + ylab(expression(f(S(t))))
   + xlab(expression(S(t)))
   + theme_bw()
)




## ----var_vax, echo=FALSE, warning=FALSE---------------------------------------
# vaccination rate phi(S(t))
(varvax 
   |> filter(matrix %in% c("vaccination"))
   |> cbind(S)
   |> ggplot(aes(S, value))
   + geom_point(alpha=0.3)
   + geom_hline(yintercept = 1,col="red")
   + geom_text(x = 1500, y = 0.98, label = "maximum rate", col = "red")
   + theme_bw()
   + ggtitle("Vaccination Rate")
   + ylab(expression(phi(S(t))))
   + xlab(expression(S(t)))
   + theme_bw()
)



## ----calibration_scenario-----------------------------------------------------
expected_daily_reports = 90 # days
missed_reports = 10
actual_daily_reports = expected_daily_reports - missed_reports


## ----observed_data------------------------------------------------------------
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

reported_hospitalizations = (daily_hospitalizations
  # remove first record (Aug 10, this will be the initial condition for H)
  |> filter(time!=1) 
  # update all times to set Aug 11 to be day 1 of the scenario
  |> mutate(time=time-1) 
  |> head(expected_daily_reports)
  |> mutate(matrix="H")
  |> sample_n(actual_daily_reports)
  |> arrange(time)
)



## ----mp_default---------------------------------------------------------------
# We can view model spec default values here to see if we need to make any
# changes
mp_default(spec)



## ----N------------------------------------------------------------------------
# N = population size
# ---------------------
N = 14.8e7


## ----phi----------------------------------------------------------------------
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


## ----other_defaults-----------------------------------------------------------
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


## ----initial_conditions-------------------------------------------------------
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
# and divide by 7 as above to make it a daily estimate. We should also multiply by something
# to account for under-reporting ... say 10.
I0 = 10 * 1903/7

# H0 = initial H
# ---------------------
# Use the first observed data point (Aug 10, 2021) as initial H.
H0 = daily_hospitalizations |> filter(row_number() == 1) |> select(value) |> pull()


## ----defaults-----------------------------------------------------------------
spec = mp_tmb_update(spec
  , default = list(
      N = N
    , V = V0
    , I = I0
    , H = H0
    , a = a
    , b = a
    , alpha = alpha
    , sigma = sigma
    , rho = rho
  ) 
)


## ----simulating_dynamics------------------------------------------------------

# state variables
states = c("S","H","I","V","E","R")

# set up calibrator
shiver_calibrator = mp_tmb_calibrator(
    spec = spec
  , data = reported_hospitalizations
  , traj = "H"
  # parameters we want to estimate (transmission rates)
  # we also want to estimate initial E
  , par = c("beta_v","beta_s","E", "sigma", "gamma_h") 
  , outputs = states
)
# print to check
shiver_calibrator

# trajectory has 90 time steps (which is what we expect)
nrow(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix == "H")
  |> select(time) 
  |> unique()
)
  

# which time steps are missing in observed data
(shiver_calibrator 
  |> mp_trajectory()
  |> filter(matrix == "H")
  |> anti_join(reported_hospitalizations, by = "time")

) 

# before optimizing, do the dynamics look reasonable? 
(shiver_calibrator 
    |> mp_trajectory()
    |> ggplot(aes(time, value))
    + facet_wrap(vars(matrix), scales = 'free')
    + geom_line()
    + theme_bw()
)


## ----estimates----------------------------------------------------------------
# optimize to estimate parameters
# this converges!
mp_optimize(shiver_calibrator)

# look at estimates with CI
est_coef = mp_tmb_coef(shiver_calibrator, conf.int=TRUE) |> round_coef_tab()
est_coef


## ----fit, echo=FALSE----------------------------------------------------------
# how does the fit compare with observed data?
(shiver_calibrator 
  |> mp_trajectory_sd(conf.int = TRUE)
  |> ggplot(aes(time, value))
  + facet_wrap(~matrix, scales = "free")
  + geom_line(aes(y=value), colour = "red")
  + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.3)
  + geom_point(data = reported_hospitalizations, aes(time, value))
  + ylim(c(0, NA))
  + theme_bw()
)


## ----reparameterization-------------------------------------------------------
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
        mp_per_capita_flow("S", "E", unvaccinated_infection ~ I * beta/N_mix)
      , mp_per_capita_flow("V", "E", vaccinated_infection ~  I * beta * p/N_mix)
    )
)


# all changes have been made
print(reparameterized_spec)


## ----reparam_calib------------------------------------------------------------
prior_distributions = list(
      log_beta = mp_uniform()
    , log_E_I_ratio = mp_uniform()
    , logit_p = mp_normal(qlogis(1/4), 8)
    , sigma = mp_uniform()
    , gamma_h = mp_uniform()
)
shiver_calibrator = mp_tmb_calibrator(
    spec = reparameterized_spec
  , data = reported_hospitalizations
  , traj = "H"
  , par = prior_distributions
  , outputs = c(states, "infection")
)

# optimize to estimate transmission parameters
# converges with warnings
mp_optimize(shiver_calibrator)


## ----reparam_estimates, echo=FALSE--------------------------------------------
# looking at coefficients and CIs
# we need to back transform to interpret
cc <- mp_tmb_coef(shiver_calibrator, conf.int = TRUE) |> round_coef_tab()
print(cc)


## ----repar_fit----------------------------------------------------------------
(shiver_calibrator 
  |> mp_trajectory_sd(conf.int = TRUE)
  |> ggplot(aes(time, value))
  + facet_wrap(~matrix, scales = "free")
  + geom_line(aes(y=value), colour = "red")
  + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.3)
  + geom_point(data = reported_hospitalizations, aes(time, value))
  + ylim(c(0, NA))
  + theme_bw()
)


## ----rk4----------------------------------------------------------------------
# let's calibrate
shiver_calibrator_rk4 = mp_tmb_calibrator(
    spec = reparameterized_spec |> mp_rk4()
  , data = reported_hospitalizations
  , traj = "H"
  , par = prior_distributions
  , outputs = c(states, "infection")
)

# optimize
# converges with warning
mp_optimize(shiver_calibrator_rk4)

# looking at coefficients and CIs
rk4_coef <- mp_tmb_coef(shiver_calibrator_rk4, conf.int = TRUE) |> round_coef_tab()

print(rk4_coef)
# rk4 doesn't help us learn more about p
# let's try adding more data



## ----multiple_traj_data, include=FALSE----------------------------------------
# COVID19 case data for Ontario
# It makes sense to assume these case counts are incidence (# number of new 
# cases each day) instead of prevalence (# all active cases each day)
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
     |> filter(time != 1 & time != 2)  # remove first two records (Aug 9, Aug10 = I0)
     |> mutate(time = time - 2)        # update all times to set Aug 11 to be day 1 of the scenario
     |> head(expected_daily_reports)
     |> mutate(matrix = "reported_incidence") # the infection variable is incidence
     |> select(c("time","value","matrix"))
)

# use Aug 10 as initial I
# Note: this case data is from a different source than above, but I0 and I0_new
# don't differ much
I0_new = daily_cases |> filter(time==2) |> select(value) |> pull()
I0_new = I0_new * 10  ## account for under-reporting a bit

# check defaults again
mp_default(reparameterized_spec)


## ----incidence_in_model-------------------------------------------------------
multi_traj_spec = (reparameterized_spec
  |> mp_tmb_insert(
      phase = "during"
    , at = Inf
    , expressions = list(incidence ~ unvaccinated_infection + vaccinated_infection)
  ) 
  
  ## with case report data, we need to account for reporting delays and 
  ## under-reporting.
  |> mp_tmb_insert_reports("incidence", report_prob = 0.1, mean_delay = 11, cv_delay = 0.25)
  
  ## we again want to fit many parameters on log or logit scales.
  |> mp_tmb_insert(phase = "before", at = 1L
    , expressions = list(
          incidence_report_prob ~ 1/(1 + exp(-logit_report_prob))
        , I ~ exp(log_I)
        , H ~ exp(log_H)
        , R ~ exp(log_R)
        , sigma ~ exp(log_sigma)
        , gamma_h ~ exp(log_gamma_h)
      )
    , default = list(
          logit_report_prob = 0
        , rbf_beta = 1
        , V = V0
        , log_I = log(I0)
        , log_H = log(H0)
        , log_R = 0
        , log_sigma = -3
        , log_gamma_h = -3
        , N = N
      )
  )
  
  ## we also need to prepare for a more flexible fit to the transmission
  ## rate that varies over time, as the report data provide sufficiently more
  ## information for this purposes.
  |> mp_tmb_insert(phase = "during"
    , at = 1L
    , expressions = list(beta ~ beta * rbf_beta)
  )
)


## ----multiple_traj_calib------------------------------------------------------
## we need a more elaborate prior distribution
sd_par = 1 ## for convenience we give all parameters the same prior sd, for now
sd_state = 4 ## extremely vague priors on state variables
prior_distributions = list(
    log_beta = mp_normal(log(0.2), sd_par)
  , log_sigma = mp_normal(log(sigma), sd_par)
  , log_gamma_h = mp_normal(log(0.07), sd_par)
  , logit_report_prob = mp_normal(qlogis(0.1), sd_par)
  , logit_p = mp_normal(qlogis(1/4), 4)
  , log_E_I_ratio = mp_normal(0, sd_par)
  , log_I = mp_normal(log(I0), sd_state)
  , log_H = mp_normal(log(H0), sd_state)
  , log_R = mp_normal(log(1), sd_state)
)

## put the data together
dd = rbind(reported_hospitalizations, reported_cases)

# calibrate
shiver_calibrator = mp_tmb_calibrator(
    spec = (multi_traj_spec 
      |> mp_hazard()
    )
    # row bind both observed data
  , data = dd
    # fit both trajectories with log-normal distributions
    # (changed from negative binomial because apparently it is easier
    # to fit standard deviations than dispersion parameters)
  , traj = list(H = mp_log_normal(sd = mp_fit(1))
    , reported_incidence = mp_log_normal(sd = mp_fit(1))
  )
  , par = prior_distributions
    # fit the transmission rate using five radial basis functions for
    # a flexible model of time variation.
  , tv = mp_rbf("rbf_beta", 5, sparse_tol = 1e-8)
  , outputs = c(states, "reported_incidence", "beta")
)


## ----mult_traj_optim, include=FALSE-------------------------------------------
# optimize to estimate transmission parameters
# converges with warnings
mp_optimize(shiver_calibrator)


## ----mult_traj_estimates, echo=FALSE------------------------------------------
#check estimates
cc = mp_tmb_coef(shiver_calibrator, conf.int = TRUE)
round_coef_tab(cc)


## ----mult_traj_fit, echo=FALSE------------------------------------------------
(shiver_calibrator
   |> mp_trajectory_sd(conf.int = TRUE)
   |> ggplot(aes(time, value))
   + geom_line(aes(y = value), colour = "red")
   + geom_ribbon(
        aes(ymin = conf.low, ymax = conf.high)
      , fill = "red"
      , alpha = 0.3
    )
   + geom_point(data = dd, aes(time, value))
   + facet_wrap(vars(matrix), scales = 'free')
   + theme_bw()
)


## ----ground_truth-------------------------------------------------------------
# set true values
true_p = 0.2
true_beta = 0.3 


## ----identifiability----------------------------------------------------------
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
mp_tmb_coef(fixed_beta, conf.int = TRUE) |> round_coef_tab()
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
mp_tmb_coef(fixed_a, conf.int = TRUE) |> round_coef_tab()

