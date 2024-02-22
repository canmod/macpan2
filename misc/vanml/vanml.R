## ----include = FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----other_packages, message=FALSE-----------------------------------------------------------------------
library(macpan2)
library(dplyr)
library(ggplot2)
library(broom.mixed)


## ----installation, message=FALSE-------------------------------------------------------------------------
if (!require(iidda.api)) {
  install.packages("iidda.api", repos = c("https://canmod.r-universe.dev", "https://cran.r-project.org"))
}
api_hook = iidda.api::ops_staging


## ----api_hook--------------------------------------------------------------------------------------------
options(iidda_api_msgs = FALSE, macpan2_verbose = FALSE)


## ----scarlet_fever_ontario-------------------------------------------------------------------------------
scarlet_fever_ontario = api_hook$filter(resource_type = "CANMOD CDI"
  , iso_3166_2 = "CA-ON"  ## get ontario data
  , time_scale = "wk"  ## weekly incidence data only 
  , disease = "scarlet-fever"
  
  # get data between 1929-08-01 and 1930-10-01
  , period_end_date = "1929-08-01/1930-10-01"  
)
print(scarlet_fever_ontario)


## ----scarlet_fever_ontario_plot, fig.width=7-------------------------------------------------------------
(scarlet_fever_ontario
  |> ggplot(aes(period_mid_date, cases_this_period))
  + geom_line() + geom_point()
  + ggtitle("Scarlet Fever Incidence in Ontario, Canada")
  + theme_bw()
)


## --------------------------------------------------------------------------------------------------------
sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
print(sir)


## --------------------------------------------------------------------------------------------------------
sf_sir = mp_tmb_insert(sir
  , default = list(N = median(scarlet_fever_ontario$population))
)


## --------------------------------------------------------------------------------------------------------
sf_sir = mp_tmb_insert(sf_sir
    ## insert this expression ...
  , expressions = list(reports ~ infection * report_prob)
    
    ## at the end (i.e. Infinity) of the expressions evaluated
    ## 'during' each iteration of the simulation loop ...
  , at = Inf  
  , phase = "during"
  
    ## add a new default value for the reporting probability
  , default = list(report_prob = 1/300)
)


## --------------------------------------------------------------------------------------------------------
print(sf_sir)


## --------------------------------------------------------------------------------------------------------
sir_simulator = mp_simulator(sf_sir
  , time_steps = 5
  , outputs = "reports"
)
head(mp_trajectory(sir_simulator))


## --------------------------------------------------------------------------------------------------------
observed_data = (scarlet_fever_ontario
  ## select the variables to be modelled -- a time-series of case reports.
  |> select(period_end_date, cases_this_period)
  
  ## change the column headings so that the match the columns
  ## in the simulated trajectories.
  |> mutate(matrix = "reports")
  |> rename(value = cases_this_period)
  
  ## create a `time` column with the time-step IDs that will correspond
  ## to the time-steps in the simulation. this column heading also 
  ## must match the column with the time-steps in the simulated trajectories
  |> mutate(time = seq_along(period_end_date))
)
print(head(observed_data))


## --------------------------------------------------------------------------------------------------------
sir_cal = mp_tmb_calibrator(
    spec = sf_sir
  , data = observed_data
  
  ## name the trajectory variable, with a name that
  ## is the same in both the spec and the data
  , traj = "reports"  
  
  ## fit the following parameters
  , par = c("beta", "gamma", "I", "report_prob")
)


## --------------------------------------------------------------------------------------------------------
print(sf_sir)


## --------------------------------------------------------------------------------------------------------
sir_opt = mp_optimize(sir_cal)


## --------------------------------------------------------------------------------------------------------
print(sir_opt)


## --------------------------------------------------------------------------------------------------------
mp_tmb_coef(sir_cal, conf.int = TRUE)


## ----fig.width=7, fig.height=7---------------------------------------------------------------------------
fitted_data = mp_trajectory_sd(sir_cal, conf.int = TRUE)
(observed_data
  |> ggplot()
  + geom_point(aes(time, value))
  + geom_line(aes(time, value)
    , data = fitted_data
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , data = fitted_data
    , alpha = 0.2
    , colour = "red"
  )
  + theme_bw()
  + facet_wrap(~matrix, ncol = 1, scales = "free")
)

