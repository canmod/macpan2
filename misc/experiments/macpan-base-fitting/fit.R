library(macpan2)
library(dplyr)
library(ggplot2)
library(tidyr)
options(macpan2_default_loss = "neg_bin") 

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
## format data
## -------------------------

# read in data
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
                    |> filter(matrix %in% c("death","report"))
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
# get time steps for specific dates
# there are two additional breaks (not specified in the paper?)
# see ML comment Figure 3.
break_times = (formatted_tsdata
 |> filter(date %in% c("2020-04-01", "2020-08-07"))
 |> select(time,date)
 |> unique()
)

# time-varying transmission change points
# time_var needs change_points to start at 0 (for now)
beta_changepoints = c(0,break_times$time)


# remove synonym (can't have both 'date' and 'time')
formatted_tsdata = formatted_tsdata |> select(-c(date))

# one negative value for daily deaths
# removing for now
formatted_tsdata = formatted_tsdata |> filter(value>=0)

## -------------------------
## update model spec (base model)
## -------------------------

time_steps = nrow(formatted_tsdata %>% select(time) %>% unique())

# logistic transition curve for breakpoints
#S_j = function(t, tau_j, s) plogis((t - tau_j) / s)
S_j = function(t, tau_j, s) 1/(1+exp((t - tau_j) / s))

# model_matrix
X = cbind(
    formatted_mobility_dat$log_mobility_ind
  , S_j(1:time_steps, beta_changepoints[2], 3)
  , S_j(1:time_steps, beta_changepoints[2], 3) * formatted_mobility_dat$log_mobility_ind
  , S_j(1:time_steps, beta_changepoints[3], 3)
  , S_j(1:time_steps, beta_changepoints[3], 3) * formatted_mobility_dat$log_mobility_ind
) %>% as.matrix()

X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)

model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index

log_model_coefficients = rep(log(1e-2),ncol(X))
#log_model_coefficients = rep(0,ncol(X))

# --------------- model spec ---------------
#macpan_base = (mp_tmb_library("starter_models","macpan_base",package="macpan2")
source("./inst/starter_models/macpan_base/tmb.R")
macpan_base = (spec 
  # add variable transformations:
  |> mp_tmb_insert(phase = "before"
    , at = 1L,
    expressions = list(
       zeta ~ exp(log_zeta)
     , beta0 ~ exp(log_beta0)
     , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
    ),
    default = list(
      log_zeta = log(1e-2)
    , log_beta0 = log(1e-2)
    , logit_nonhosp_mort = log((1e-2)/(1-(1e-2)))
    ) 
  )
               
  # add accumulator variables:
  # H2 - individuals in acute care after discharge (already computed)
  # D - cumulative deaths (already computed)
  # X - cumulative hospital admissions
  # death - new deaths each time step
  |> mp_tmb_insert(phase = "during"
    , at = Inf
    , expressions = list(
        mp_per_capita_flow("H", "X", H.X ~ Is.ICUs + Is.ICUd)
      , death ~ ICUd.D * ICUd + Is.D * Is
      )
    , default = list(X = 0)
  )

  # add phenomenological heterogeneity:
  |> mp_tmb_update(phase = "during"
    , at =1L
    , expressions = list(mp_per_capita_flow("S", "E", S.E ~ (S^(zeta-1)) * (beta / (N^zeta)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
    , default = list(beta = 1)
  )
 
  # add condensation variables:
  # I = sum of all infectious compartments (time step incidence)
  |> mp_tmb_insert(phase = "during"
    , at = Inf
    , expressions = list(I ~ Ia + Ip + Im + Is) ## this is probably wrong, Ip -> Im & Ip -> Is
    , default = list(I = 0)
  )
  # add incidence:
  |> mp_tmb_insert(phase = "during"
    , at = Inf
    #, expressions = list(incidence ~ S.E * (S^2))
    , expressions = list(incidence ~ S.E * S)
    , default = list(incidence = 0)
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
    , default = list(qmax = 16)
  )
  
  # add convolution to compute case reports:
  |> mp_tmb_insert(phase = "during"
    , at = Inf
    , expressions = list(report ~ convolution(incidence, kappa))
    , default = list(report = 0)
  )
   ##|> mp_tmb_insert(phase = "after"
    # , at = 1L
    # remove beginning time steps depending on kernel length
    # , expressions = list(report ~ rbind_time(report, i))
    # , integers = list(i = 16:(time_steps-1))
  #   , expressions = list(report ~ rbind_time(report, 1:189), report[i] ~ 0)
  #   , integers = list(i = c(0:14))
  # )
  
  # add time-varying transmission with mobility data:
  |> mp_tmb_insert(phase = "before"
    , at = Inf
    , expressions = list(relative_beta_values ~ exp(group_sums(model_matrix_values * log_model_coefficients[col_ind], row_ind, model_matrix_values)))
    , default = list(log_model_coefficients = log_model_coefficients, model_matrix_values = model_matrix_values, relative_beta_values=model_matrix_values)
    , integers = list(row_ind = row_ind, col_ind = col_ind)
  )
  |> mp_tmb_insert(phase = "during"
    , at = 1L
    , expressions = list(
       beta ~ beta0 * relative_beta_values[time_step(1)]
      )
  )
)
# ------------------------------------------

## simulate data
(macpan_base
  %>% mp_simulator(time_steps = time_steps, outputs = c("Ia","Ip","Im","report"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)

(macpan_base
  %>% mp_simulator(time_steps = time_steps, outputs = c("S","E","death","report"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)
(macpan_base
  %>% mp_simulator(time_steps = time_steps, outputs = c("incidence","report"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)

(macpan_base
  %>% mp_simulator(time_steps = 1L, outputs = c("kappa"))
  %>% mp_trajectory()
  %>% select(value)
  %>% pull()
  %>% plot(., type='l')
)


## -------------------------
## setting initial state vector
## -------------------------

# simulate from model with no susceptible depletion for 100 time steps
# final states should be close to eigenvector

# update model spec to remove outflow from S
# think the easiest way is reset S to N each time step
macpan_base_initialgrowth = mp_tmb_update(macpan_base
  , phase = "during"
  , at = 2L
  , expressions = list(S.E ~ (S^(zeta-1)) * (beta / (N^zeta)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
  
)
macpan_base_initialgrowth = mp_tmb_insert(macpan_base_initialgrowth
  , phase = "during"
  , at = 3L
  , expressions = list(E ~ E + S.E * S)
                                          
)
(macpan_base_initialgrowth
  %>% mp_simulator(time_steps = 100L, outputs = c("S","E","Ip","Ia","Is","Im","ICUs","ICUd","H","H2","R","D"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)

initial_state_vector = (
  (mp_final(macpan_base_initialgrowth
  # I think these are the only relevant compartments? or do we need to set all compartments
  # including those not involved in transmission, R & D?
  |> mp_simulator(time_steps = 100L, outputs = c("E","Ip","Ia","Is","Im","ICUs","ICUd","H","H2","R","D")))
  # scale by 1% of the population                      
  ) |> mutate(scaled_value = value * mp_default_list(macpan_base)$S * 0.01)
) |> select(matrix, scaled_value) |> pivot_wider(names_from = matrix, values_from=scaled_value) 

sum(initial_state_vector)


## -------------------------
## initial calibration
## -------------------------
states = c("S","E","Ip","Ia","Is","Im","ICUs","ICUd","H","H2","R","D")

# simulate from model with fixed beta0
sim_data = (mp_simulator(macpan_base |> mp_rk4()
            , time_steps = 100L
            , outputs = c("Is","H","R","D")
            , default = list(
                logit_nonhosp_mort = qlogis(0.3)
              , E = 1000
            )
) |> mp_trajectory()
) 
sim_data %>% filter(time < 5, matrix =='Is')

# look at simulated data
(sim_data
  |> ggplot(aes(time,value))+geom_point()+facet_wrap(vars(matrix), scales= "free")
)

# calibrate to see if we can recover nonhosp_mort
mb_calib = mp_tmb_calibrator(
  spec = macpan_base |> mp_rk4()
  , data = sim_data
  , traj =  c("Is","H","R","D")
  , par = c("logit_nonhosp_mort"
  ) 
  , default = list(E = 1000)
)

mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(mb_calib, conf.int = TRUE) |> backtrans()


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
  + facet_wrap(vars(matrix), scales = "free")
  + theme_bw()
)


# simulate a single individual (E = 1, all other states 0) 
sim_one = (mp_simulator(macpan_base
  , time_steps = 5L
  , outputs = c("S.E")
  , default = list(E = 1, S = 1e-8, log_beta0 = log(1))
  ) 
 |> mp_trajectory()
) ##|> filter(value > 0)

# R_0 (for this parameter set) is sum of all compartments?
sum(sim_one$value)


# solve for growth rate r
euler_lotka = function(r) sum(sim_one$value * exp(-r * sim_one$time)) - 1

uniroot(euler_lotka, c(0,Inf))
poly_coefs = c(1, sim_one %>% group_by(time) %>% summarize(value = sum(value)) %>% pull(value))
polyroot(poly_coefs) # why are they complex



## -------------------------
## calibration (base model)
## -------------------------

# fitting to deaths and reports
mb_calib = mp_tmb_calibrator(
    spec = macpan_base |> mp_rk4()
  , data = formatted_tsdata
  , traj = c("report","death") 
  ## Table 1 in manuscript
  , par = c(
      "E"
    # , "Ia"
    # , "Ip"
    # , "Im"
    # , "Is"
    , "log_zeta"
    , "log_beta0"
    , "log_model_coefficients"
    , "logit_nonhosp_mort" #I think this is eta - 'Probability of mortality without hospitalization'
    ) 
  , outputs = c("S","E","I","death","report")
  , default = list(
      E = initial_state_vector$E
    , Ia = initial_state_vector$Ia
    , Ip = initial_state_vector$Ip
    , Im = initial_state_vector$Im
    , Is = initial_state_vector$Is
    #, H = initial_state_vector$H
    #, ICUs = initial_state_vector$ICUs
    #, ICUd = initial_state_vector$ICUd
    #, H2 = initial_state_vector$H2
    #, R = initial_state_vector$R
    #, D = initial_state_vector$D
  )
)

mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(mb_calib, conf.int = TRUE) |> backtrans()


(ggplot(formatted_tsdata, aes(time,value))
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

(ggplot(fitted_data |> filter(matrix %in% c("S","E","I")), aes(time,value))
  + geom_point()
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)


