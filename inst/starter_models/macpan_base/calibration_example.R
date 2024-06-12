library(macpan2)
library(dplyr)
library(ggplot2)
library(tidyr)
options(macpan2_default_loss = "neg_bin") 

## -------------------------
## Example Set-up
## -------------------------

# This calibration example reproduces the base model calibration fit from:
# https://github.com/mac-theobio/macpan_base, originally performed with the
# McMasterPandemic package.

# The base model calibration was 

## -------------------------
## Observed Data Prep
## -------------------------

# Observed data is sourced from the following data prep script, which can be
# slow to run. Once sourced, the following RDS files contain the relevant time
# series and mobility data.
#source(system.file("starter_models","macpan_base","data","get_data.R", package = "macpan2"))
ts_data = readRDS(system.file("starter_models","macpan_base","data","ts_data.RDS",package = "macpan2"))
mobility_data = readRDS(system.file("starter_models","macpan_base","data","mobility_data.RDS",package = "macpan2"))

# To further prepare the time series data for calibration we filter for the 
# appropriate time range and time series variables, create a numeric date field
# named 'time'.
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
# To further prepare the mobility data for calibration we filter for the 
# appropriate time range, create a numeric date field named 'time' and compute 
# the logarithm of the mobility index.
prepped_mobility_data = (mobility_data
  # dates from base model calibration (Figure 4)
  |> filter(date >= "2020-02-24" & date < "2020-08-31")
  # create unique time identifier
  |> arrange(date)
  |> group_by(date)
  |> mutate(time = cur_group_id())
  |> ungroup()
  |> mutate(log_mobility_ind = log(mobility_ind))
)

# Mobility breakpoints identified in the manuscript for piecewise varying
# transmission.
mobility_breaks = (prepped_ts_data
  |> filter(date %in% c("2020-04-01", "2020-08-07"), matrix=="report")
  |> pull(time)
)

# Maximum number of time steps in the time series.
time_steps = nrow(prepped_ts_data %>% select(date) %>% unique())

# Drop time synonym.
prepped_ts_data = select(prepped_ts_data, -date)

# logistic transition curve for mobility breakpoints
S_j = function(t, tau_j, s) 1/(1 + exp((t - tau_j) / s))

# Model matrix (X in manuscript) encapsulating the 
X = cbind(
      prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[1], 3)
    , S_j(1:time_steps, mobility_breaks[1], 3) * prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[2], 3)
    , S_j(1:time_steps, mobility_breaks[2], 3) * prepped_mobility_data$log_mobility_ind
    ) %>% as.matrix()

# get model matrix meta data for simplified matrix multiplication in
# macpan2 using group_sums
X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)
model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index


## -------------------------
## Model Specification
## -------------------------

# get model spec from library
macpan_base = mp_tmb_library("starter_models","macpan_base",package="macpan2")

# Update model specificaiton to include additional components described in
# manuscript and required for calibration.
focal_model = (macpan_base 
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
      , expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                    
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



mp_simulator(focal_model, 100, "S.E") #%>% mp_trajectory()

## -------------------------
## calibration
## -------------------------



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
  )
  , outputs = c("death","report","S.E")
  , default = list(
    
    # states  
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
    
    , model_matrix_values = model_matrix_values
    
    # set initial parameter values for optimizer
    , qmax = 21
    , log_beta0=log(0.1)
    , logit_nonhosp_mort = qlogis(0.1)
    , log_mobility_coefficients = rep(0,5)
    , log_zeta = 0
    
  )
)

mp_optimize(focal_calib)

# get fitted data
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(focal_calib, conf.int = TRUE) |> backtrans()

(ggplot(formatted_tsdata_pre, aes(time,value))
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







## -------------------------
## simulate fake data
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# Ca value to simulate data with
true_Ca = 0.8

# infectious compartment names
I_labels = c("Ia", "Ip", "Im", "Is")

# simulator object
macpan_base = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = I_labels
  , default = list(Ca = true_Ca)
)

# simulated data
sim_data = (macpan_base 
            |> mp_trajectory() 
            |> mutate(across(value, ~ rpois(n(), .)))
)

# visualize simulated prevalence
if (interactive()) {
  ggplot(sim_data)+
    geom_point(aes(time,value))+
    theme_bw()+
    facet_wrap(vars(matrix),scales = 'free')
  
}


## -------------------------
## parameterize model
## -------------------------

mb_calib = mp_tmb_calibrator(
  # add log transformation for Ca parameter
    spec = (spec 
      |> mp_tmb_insert(phase = "before", at = 1L
                       , expressions = list(Ca ~ exp(log_Ca))
                       , default = list(log_Ca = log(1e-2)))
    )
  , data = sim_data
  , traj = I_labels
  , par = "log_Ca"
)

## -------------------------
## fit model
## -------------------------

# optimize
mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate, close to true
mp_tmb_coef(mb_calib) %>% mutate(estimate = exp(estimate))

# fit looks reasonable
if (interactive()) {
(ggplot(sim_data, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data
              , colour = "red"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data
                , alpha = 0.2
                , colour = "red"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
  + ylab("prevalence")
)
}


## -------------------------
## exploring
## -------------------------

## plotting Ia(t) vs. Ia(t+1) - should be linear?
if (interactive()) {
  
  Ia_shifted <- (macpan_base
                 %>% mp_trajectory() 
                 %>% filter(matrix=="Ia") 
                 %>% mutate(Ia_lead = lead(value))
                 %>% rename(Ia=value)
                )
  
  # linear model might not be appropriate
  # what are the assumptions about error here?
  lm_fit <- lm(data = Ia_shifted,Ia_lead ~ Ia)
  summary(lm_fit)
  
  ggplot(Ia_shifted, aes(Ia, Ia_lead))+
    geom_line()+
    
    #add lm
    # geom_abline(intercept = lm_fit$coefficients[1],
    #             slope = lm_fit$coefficients[2], col="red")+
  
    geom_smooth(method = "lm", se = FALSE, col="blue")+
    theme_bw()
  
}


## all infectious compartments
if (interactive()) {
  ggplot(macpan_base %>% mp_trajectory() %>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}
