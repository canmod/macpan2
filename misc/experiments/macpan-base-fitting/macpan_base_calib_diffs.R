library(macpan2)
library(dplyr)
library(ggplot2)
library(tidyr)
options(macpan2_default_loss = "neg_bin")

## -------------------------
## time series data prep
## -------------------------
#source('./misc/experiments/macpan-base-fitting/data.R') #~ 2GB of data
clean_tsdata = readRDS("./misc/experiments/macpan-base-fitting/clean_tsdata.RDS")
mobility_dat = readRDS("./misc/experiments/macpan-base-fitting/mobility_dat.RDS")

# get data in the right format for calibration
formatted_tsdata = (clean_tsdata
                    |> rename(matrix=var)
                    # dates from base model calibration (Figure 4)
                    |> filter(date >= "2020-02-24" & date < "2020-08-31")
                    |> arrange(date)
                    |> group_by(date)
                    |> mutate(time = cur_group_id())
                    |> ungroup()
                    # one negative value for daily deaths (removing for now time==178)
                    # this explains negative values:
                    # https://github.com/ccodwg/Covid19Canada?tab=readme-ov-file#datasets
                    |> filter(matrix %in% c("death","report") & value>=0)
)
formatted_mobility_dat = (mobility_dat
                          |> filter(date >= "2020-02-24" & date < "2020-08-31")
                          |> mutate(log_mobility_ind = log(mobility_ind))
                          |> arrange(date)
                          |> group_by(date)
                          |> mutate(time = cur_group_id())
                          |> ungroup()
                          ##|> pull(log_mobility_ind)
)
# get time steps for specific mobility breakpoints
mobility_breaks = (formatted_tsdata
                   |> filter(date %in% c("2020-04-01", "2020-08-07"))
                   |> select(time,date)
                   |> unique()
                   |> pull(time)
)

# remove synonym (can't have both 'date' and 'time')
formatted_tsdata = formatted_tsdata |> select(-c(date))

## -------------------------
## mobility data prep
## -------------------------

# time steps in observed data
time_steps = nrow(formatted_tsdata %>% select(time) %>% unique())

# logistic transition curve for breakpoints
#S_j = function(t, tau_j, s) plogis((t - tau_j) / s)
S_j = function(t, tau_j, s) 1/(1+exp((t - tau_j) / s))

# model matrix (X in manuscript)
X = cbind(
  formatted_mobility_dat$log_mobility_ind
  , S_j(1:time_steps, mobility_breaks[1], 3)
  , S_j(1:time_steps, mobility_breaks[1], 3) * formatted_mobility_dat$log_mobility_ind
  , S_j(1:time_steps, mobility_breaks[2], 3)
  , S_j(1:time_steps, mobility_breaks[2], 3) * formatted_mobility_dat$log_mobility_ind
) %>% as.matrix()

## -------------------------
## focal model
## -------------------------

spec = mp_tmb_library("starter_models","macpan_base",package="macpan2")
# the epi-model we are interested in
focal_model = (spec 
               # add variable transformations:
               |> mp_tmb_insert(phase = "before"
                                , at = 1L,
                                expressions = list(
                                    zeta ~ exp(log_zeta)
                                  , beta0 ~ exp(log_beta0)
                                  , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
                                  , E ~ exp(log_E)
                                ), 
                                default = list(
                                    log_zeta = log(1e-2)
                                  , log_beta0 = log(1e-2)
                                  , logit_nonhosp_mort = log(0.1/(1-0.1))
                                  , log_E = log(1)
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
                                , default = list(beta = 1)
               )
               
               # add incidence:
               |> mp_tmb_insert(phase = "during"
                                , at = Inf
                                , expressions = list(incidence ~ S * ((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
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
                                , default = list(qmax = 34)
               )
               
               # # add convolution to compute case reports:
               |> mp_tmb_insert(phase = "during"
                                , at = Inf
                                , expressions = list(report ~ convolution(incidence, kappa))
               )
               
               # add time-varying transmission with mobility data:
               |> mp_tmb_insert(phase = "before"
                                , at = Inf
                                #, expressions = list(relative_beta_values ~ exp(group_sums(model_matrix_values * log_mobility_coefficients[col_ind], row_ind, model_matrix_values)))
                                , expressions = list(relative_beta_values ~ X %*% log_mobility_coefficients) #X * c (to simplify model as I diagnose calibration issues)
                                , default = list(log_mobility_coefficients = rep(0,5), X=X)
                                #, integers = list(row_ind = row_ind, col_ind = col_ind)
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

first_exp = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = formatted_tsdata
  , traj = c("report","death") 
  , par = c("log_beta0","logit_nonhosp_mort","log_mobility_coefficients")
  , outputs = c("death","report","incidence")
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
    # set initial parameter values for optimizer
    , qmax = 21
    , log_beta0=log(0.1)
    , logit_nonhosp_mort = qlogis(0.1)
    , log_mobility_coefficients = rep(0,5)
    , log_zeta = 0
  )
)
second_exp = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = formatted_tsdata
  , traj = c("report","death") 
  , par = c("log_beta0","logit_nonhosp_mort","log_mobility_coefficients")
  , outputs = c("death","report")
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
    # set initial parameter values for optimizer
    , qmax = 21
    , log_beta0=log(0.1)
    , logit_nonhosp_mort = qlogis(0.1)
    , log_mobility_coefficients = rep(0,5)
    , log_zeta = 0
  )
)
mp_optimize(first_exp)
mp_optimize(second_exp)

# get fitted data
fitted_data_first = mp_trajectory_sd(first_exp, conf.int = TRUE)
fitted_data_second = mp_trajectory_sd(second_exp, conf.int = TRUE)

# check estimate
mp_tmb_coef(first_exp, conf.int = TRUE) #|> backtrans()
mp_tmb_coef(second_exp, conf.int = TRUE) #|> backtrans()

(ggplot(formatted_tsdata, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data_first |> filter(matrix %in% c("death","report"))
              , colour = "red"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data_first |> filter(matrix %in% c("death","report"))
                , alpha = 0.2
                , colour = "red"
  )
  + geom_line(aes(time, value)
              , data = fitted_data_second |> filter(matrix %in% c("death","report"))
              , colour = "blue"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data_second |> filter(matrix %in% c("death","report"))
                , alpha = 0.2
                , colour = "blue"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)

