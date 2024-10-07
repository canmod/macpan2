library(macpan2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(piggyback)
options(macpan2_default_loss = "neg_bin") 

## -------------------------
## Example Set-up
## -------------------------

# This calibration example reproduces the base model calibration fit from:
# https://github.com/mac-theobio/macpan_base, originally performed with the
# McMasterPandemic package.

# The base model calibration was fit to observed death and case reports. 

# The term 'manuscript' used below refers to the macpan_ms.pdf output generated
# here: https://github.com/mac-theobio/macpan_base/tree/main/outputs

## -------------------------
## Observed Data Prep
## -------------------------

# Observed Ontario COVID-19 data
#ts_data  = readRDS(url(piggyback::pb_download_url("covid_on.RDS","canmod/macpan2"))) ## remove piggy back dependency
ts_data  = readRDS(url("https://github.com/canmod/macpan2/releases/download/macpan1.5_data/covid_on.RDS"))


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

# Model matrix (called 'X' in manuscript) describing the temporal change in 
# transmission using mobility data and piecewise breaks smoothed with the
# logistic curve.
X = cbind(
      prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[1], 3)
    , S_j(1:time_steps, mobility_breaks[1], 3) * prepped_mobility_data$log_mobility_ind
    , S_j(1:time_steps, mobility_breaks[2], 3)
    , S_j(1:time_steps, mobility_breaks[2], 3) * prepped_mobility_data$log_mobility_ind
    ) %>% as.matrix()

# Get model matrix meta data for simplified matrix multiplication inside
# macpan2 using group_sums.
X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)
model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index


## -------------------------
## Model Specification
## -------------------------

# get model spec from library
macpan_base = mp_tmb_library("starter_models","macpan_base",package="macpan2")

# Update model specification to include additional components described in
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

## -------------------------
## calibration
## -------------------------

# Note this is not an identical calibration to the calibration performed in the
# manuscript. We use Runge-Kutta 4 here for instance, instead of hazard correction.
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

# Get fitted data
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)

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

if (interactive()) {
# Comparable to Figure 4 in manscript, which also had a poorer fit to deaths.
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
}



## -------------------------
## computing R0 with a cohort model
## -------------------------

# Simulate a single exposed individual through time (N=1, E=1, all other states 
# 0). The transmission kernel is the force of infection at each time step of the 
# simulation. We sum the transmission kernel to get an estimate on R0 (for a 
# given set of parameters?)

# In our case, our focal model includes phenomenological heterogeneity and 
# setting S to 0, leads to 0^0 issues. SW said: "The way out, I think, is to 
# realize that the kernel method assumes we are at the beginning of the epidemic 
# and therefore that S/N ~ 1.  In this case, FOI reduces to beta * I / N, which 
# presents no issue.  So we do not need safe_power."

# Should we turn off phenomenological heterogeneity, or turn off inflow to E?

# Turn off inflow to E:
# update focal model model to remove inflow to E (ensuring  no new susceptibles 
# reach E)
# this update also automatically turns off outflow from S, which is also what we
# want because this makes S/N ~ 1
# In this case we don't initialize S to 0. We have N = N_focal (full population),
# N_cohort = cohort population (1 in this case)
cohort_model_ph = (
  focal_model
  %>% mp_tmb_update(phase="during"
                    , at = 2L
                    , expressions = list(S.E ~ (S^zeta) * (beta / (N^(zeta) * N_cohort)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
                    , default = list(S.E = 0))
)

# how long to simulate for?
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

R0_ph = sum(cohort_sim_ph$value)

# Turn off phenomenological heterogeneity:
# update S.E flow to remove phenomenological heterogeneity
# add duplicate foi expression because we can't recover the foi = S.E / S, when 
# S is 0.
# In this case we do initialize S to 0.
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

# arrive at approximately the same answer
all.equal(R0_ph, R0)

# solve for intrinsic growth rate r
euler_lotka = function(r) sum(cohort_sim$value * exp(-r * cohort_sim$time)) - 1

uniroot(euler_lotka, c(0,10))



## -------------------------
## exploring
## -------------------------

## plotting Ia(t) vs. Ia(t+1) - should be linear?
if (interactive()) {
  
  Ia_shifted <- (macpan_base
                 %>% mp_simulator(100,"Ia")
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

