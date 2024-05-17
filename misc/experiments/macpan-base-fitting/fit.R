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
## format observed data
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
  # one negative value for daily deaths (removing for now time==178)
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

# get model matrix meta data for simplified matrix multiplication in
# macpan2 using group_sums
X_sparse = macpan2:::sparse_matrix_notation(X, TRUE)
model_matrix_values = X_sparse$values
row_ind = X_sparse$row_index
col_ind = X_sparse$col_index

log_mobility_coefficients = rep(log(1e-2),ncol(X))


## -------------------------
## initializing variables
## -------------------------

states = c("S","E","Ip","Ia","Is","Im","ICUs","ICUd","H","H2","R","D")
intermediate_states = states[!(states %in% c("S","R","D"))]
default_states = states[!(states %in% c("S","R","D","E"))]

# N in focal model (population of Ontario 2016)
N_focal = 13448494

# percent of population in initial cohort - initial distribution of individuals 
# across all states other than S, R and D
initial_state_prop = 0.01

# set initial states for exp_growth_model (to generate initial state vector for
# focal model proportional to eigenvector of linearized model)
E0 = 1e-5
S0 = 1-E0

# parameter vector we want to optimize over (Table 1 in manuscript), is this the
# complete list of parameters?
# I think eta = nonhosp_mort = 'Probability of mortality without hospitalization'
# believe distr_params is already on log scale
theta_names = c(
    "log_zeta"
  , "log_beta0"
  , "logit_nonhosp_mort"
  , "log_mobility_coefficients" #c vector in manuscript
  , "log_E"
  , "distr_params" # negative binomial dispersion for deaths and reports
)
theta = c(
    log(35)
  , log(1)
  , qlogis(1e-2)
  , c(0.751,-0.786,0.175,-0.469,-1.496)
  , log(8.5)
  , rep(log(1000),2)
)

# theta = c(
#     log(1e-2)
#   , log(1e-2)
#   , log((1e-2)/(1-(1e-2)))
#   , log_mobility_coefficients
#   , log(1)
#   , rep(log(1),2)
# )

## -------------------------
## model spec (base model)
## -------------------------

# --------------- model spec ---------------
#macpan_base = (mp_tmb_library("starter_models","macpan_base",package="macpan2")
source("./inst/starter_models/macpan_base/tmb.R")

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
     # , Ia ~ exp(log_Ia)
     # , Ip ~ exp(log_Ip)
     # , Im ~ exp(log_Im)
     # , Is ~ exp(log_Is)
    ), 
    default = list(
      log_zeta = log(1e-2)
    , log_beta0 = log(1e-2)
    , logit_nonhosp_mort = log((1e-2)/(1-(1e-2)))
    , log_E = log(1)
    # , log_Ia = log(1)
    # , log_Ip = log(1)
    # , log_Im = log(1)
    # , log_Is = log(1)
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
      #  mp_per_capita_flow("H", "X", H.X ~ Is.ICUs + Is.ICUd)
      #, death ~ ICUd.D * ICUd + Is.D * Is
       death ~ ICUd.D + Is.D 
      )
    #, default = list(X = 0)
  )

  # add phenomenological heterogeneity:
  |> mp_tmb_update(phase = "during"
    , at =1L
    #, expressions = list(mp_per_capita_flow("S", "E", S.E ~ (S^zeta) * (beta / (N^(zeta+1))) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
    , expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
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
    #, expressions = list(incidence ~ S.E * S)
    , expressions = list(foi ~ S.E / S, incidence ~ S.E)
    #, expressions = list(foi ~ S.E)
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
    #, expressions = list(relative_beta_values ~ exp(group_sums(model_matrix_values * log_mobility_coefficients[col_ind], row_ind, model_matrix_values)))
    , expressions = list(relative_beta_values ~ X %*% log_mobility_coefficients) #Xc
    , default = list(log_mobility_coefficients = log_mobility_coefficients, model_matrix_values = model_matrix_values, relative_beta_values=model_matrix_values, X=X)
    , integers = list(row_ind = row_ind, col_ind = col_ind)
  )
  |> mp_tmb_insert(phase = "during"
    , at = 1L
    , expressions = list(
       beta ~ exp(log_beta0 + relative_beta_values[time_step(1)])
       #beta ~ beta0 * relative_beta_values[time_step(1)]
      )
  )
)

## simplified focal model with only phenomenological heterogeneity
ph_model = (spec
  # add variable transformations:
  |> mp_tmb_insert(phase = "before"
                   , at = 1L,
                   expressions = list(
                       zeta ~ exp(log_zeta)
                     , beta0 ~ exp(log_beta0)
                     , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
                     , E ~ exp(log_E)
                     # , Ia ~ exp(log_Ia)
                     # , Ip ~ exp(log_Ip)
                     # , Im ~ exp(log_Im)
                     # , Is ~ exp(log_Is)
                     
                   ),
                   default = list(
                       log_zeta = log(1e-2)
                     , log_beta0 = log(1e-2)
                     , logit_nonhosp_mort = log((1e-2)/(1-(1e-2)))
                     , log_E = log(1)
                     # , log_Ia = log(1)
                     # , log_Ip = log(1)
                     # , log_Im = log(1)
                     # , log_Is = log(1)
                   ) 
  )
  # add PH
  |> mp_tmb_update(phase = "during"
                 , at =1L
                 #, expressions = list(mp_per_capita_flow("S", "E", S.E ~ (S^zeta) * (beta / (N^(zeta+1))) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                 , expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                 , default = list(beta = 1)
                 
  )
)


# ------------------------------------------

## simulate data
(focal_model
  %>% mp_simulator(time_steps = time_steps, outputs = c("Ia","Ip","Im","report"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)

(focal_model
  %>% mp_simulator(time_steps = time_steps, outputs = c("S","E","S.E","death","report", "Ia","Ip","Im","Is","beta"))
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)
(focal_model
  %>% mp_simulator(time_steps = time_steps, outputs = c("incidence","report" ,"foi")
                   , default = list(S = N_focal
                                    , log_E = log(1e3)
                                    , Ia = 1e4)
                   )
  %>% mp_trajectory()
  %>% ggplot(aes(time,value))+
    geom_line(aes(colour=matrix))+
    facet_wrap(vars(matrix),scales = 'free')
)

(focal_model
  %>% mp_simulator(time_steps = 1L, outputs = c("kappa"))
  %>% mp_trajectory()
  %>% select(value)
  %>% pull()
  %>% plot(., type='l')
)

(spec
  %>% mp_simulator(time_steps = 20L, outputs = c("S.E"), default = list(S=0))
  %>% mp_trajectory()
  %>% select(value)
  %>% pull()
  %>% plot(., type='l')
)


## -------------------------
## initial calibration
## -------------------------

# Check that we can recover a known parameter by simulating from the model with 
# this known parameter value, and calibrating to this data.

# simulate from model with fixed logit_nonhosp_mort
sim_data = (mp_simulator(focal_model |> mp_rk4()
                         , time_steps = 100L
                         , outputs = states#c("E","Is","Im","Ip","Ia")
                         , default = list(
                             logit_nonhosp_mort = qlogis(0.3) # true value
                           , S = N_focal*(1-initial_state_prop)
                           , log_E = log(N_focal*initial_state_prop)
                           , Ia = 0
                           , Ip = 0
                           , Im = 0
                           , Is = 0
                           , H = 0
                           , ICUs = 0
                           , ICUd = 0
                           , H2 = 0
                           , R = 0
                           , D = 0
                           , log_beta0 = log(1.1)
                           , log_zeta = log(25)
                           , log_mobility_coefficients = rep(log(0.5),5)
                         )
                        ) |> mp_trajectory()
) 


# look at simulated data
(sim_data
  |> ggplot(aes(time,value))+geom_point()+facet_wrap(vars(matrix), scales= "free")
)

# calibrate to see if we can recover nonhosp_mort
initial_calib = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = sim_data
  , traj =  states #c("E","Is","Im","Ip","Ia")
  , par = c("logit_nonhosp_mort") 
  , default = list(
      # all defaults should match sim_data
      S = N_focal*(1-initial_state_prop)
    , log_E = log(N_focal*initial_state_prop)
    , Ia = 0
    , Ip = 0
    , Im = 0
    , Is = 0
    , H = 0
    , ICUs = 0
    , ICUd = 0
    , H2 = 0
    , R = 0
    , D = 0
    , log_beta0 = log(1.1)
    , log_zeta = log(25)
    , log_mobility_coefficients = rep(log(0.5),5)
  )
)

# doesn't converge (too many distributional parameters to fit?)
mp_optimize(initial_calib)

# get fitted data
fitted_data = mp_trajectory_sd(initial_calib, conf.int = TRUE)

# check estimate, estimate is close
mp_tmb_coef(initial_calib, conf.int = TRUE) |> backtrans()

# fit looks good, except why does fit get worse for D over time?
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
# update phenomenological heterogeneity model to remove inflow to E (ensuring 
# no new susceptibles reach E)
# this update also automatically turns off outflow from S, which is also what we
# want because this makes S/N ~ 1
# In this case we don't initialize S to 0. We have N = N_focal (full population),
# N_cohort = cohort population (1 in this case)
cohort_model_ph = (
  ph_model
  %>% mp_tmb_update(phase="during"
      , at = 1L
      , expressions = list(foi ~ (S^zeta) * (beta / (N^(zeta) * N_cohort)) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
      , default = list(foi = 0))
)

# how long to simulate for?
cohort_sim_ph = (mp_simulator(cohort_model_ph
  , time_steps = 100L
  , outputs = "foi"
  , default = list(log_E = log(1), N_cohort = 1, S = N_focal - 1)
  ) |> mp_trajectory()
) 

R0_ph = sum(cohort_sim_ph$value)

# Turn off phenomenological heterogeneity:
# update S.E flow to remove phenomenological heterogeneity
# add duplicate foi expression because we can't recover the foi = S.E / S, when 
# S is 0.
# In this case we do initialize S to 0.
cohort_model = (
  ph_model
  %>% mp_tmb_update(phase="during"
                    , at = 1L
                    , expressions = list(mp_per_capita_flow("S", "E", S.E ~ (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
  )
  %>% mp_tmb_insert(phase="during"
                    , at = Inf
                    , expressions = list(foi ~ (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
  )
)

cohort_sim = (mp_simulator(cohort_model
   , time_steps = 100L
   , outputs = "foi"
   , default = list(log_E = log(1), S=0)
   ) |> mp_trajectory()
) 

R0 = sum(cohort_sim$value)

# arrive at approximately the same answer
all.equal(R0_ph, R0)

# solve for growth rate r
euler_lotka = function(r) sum(cohort_sim$value * exp(-r * cohort_sim$time)) - 1

uniroot(euler_lotka, c(0,10))



## -------------------------
## eigenvector state initialization
## -------------------------

# Followed steps from: https://github.com/canmod/macpan2/issues/203 in addition 
# to turning off outflow from S, but maintaining inflow to E

# Manuscript says to simulate from model with no susceptible depletion for 100 
# time steps, and final state vector should be close to eigenvector of 
# linearized model.


exp_growth_model = (
  ph_model
  %>% mp_tmb_update(phase = "during"
                    , at = 1L
                    , expressions = list(foi ~ (S^zeta) * (beta / (N^(zeta+1))) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s)))
                    , default = list(foi = 0)
                    )
  %>% mp_tmb_insert(phase = "during"
                    , at = 2
                    , expressions = list(E ~ E + foi * S)
                    )
  %>% mp_tmb_insert(phase = "during"
                    , at = Inf
                    # normalize everything at each step (so it doesn't blow up)
                    , expressions = list(
                        S ~ S / N
                      , E ~ E / N
                      , Ia ~ Ia / N
                      , Ip ~ Ip / N
                      , Im ~ Im / N
                      , Is ~ Is / N
                      , H ~ H / N
                      , H2 ~ H2 / N
                      , ICUs ~ ICUs / N
                      , ICUd ~ ICUd / N
                      , D ~ D / N
                      , R ~ R / N
                      )
  )
  %>% mp_tmb_insert(phase="after"
                    , at = 1L
                    , expressions = list(
                      # sum all states except S, R and D
                        intermediate_state_sum ~ sum(E, Ia, Ip, Im, Is, ICUs, ICUd, H, H2)
                      # compute the number of individuals to be distributed
                      # across the intermediate states
                      , intermediate_N ~ initial_state_prop * N_focal
                      
                      # normalize and scale intermediate states
                      # there might be a way to vectorize this?
                      , E ~ intermediate_N * E/intermediate_state_sum
                      , Ia ~ intermediate_N * Ia/intermediate_state_sum
                      , Ip ~ intermediate_N * Ip/intermediate_state_sum
                      , Im ~ intermediate_N * Im/intermediate_state_sum
                      , Is ~ intermediate_N * Is/intermediate_state_sum
                      , ICUs ~ intermediate_N * ICUs/intermediate_state_sum
                      , ICUd ~ intermediate_N * ICUd/intermediate_state_sum
                      , H ~ intermediate_N * H/intermediate_state_sum
                      , H2 ~ intermediate_N * H2/intermediate_state_sum
                      
                      # update other states (might not be necessary here)
                      , S ~ N_focal - intermediate_N
                      , D ~ 0
                      , R ~ 0
                      )
                    , default = list(
                        initial_state_prop = initial_state_prop
                      , S = S0
                      , log_E = log(E0)
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
                      , N_focal = N_focal)
                    )
)


## -------------------------
## calibration of focal model (base model)
## -------------------------


# fitting to deaths and reports
focal_calib = mp_tmb_calibrator(
    spec = focal_model |> mp_rk4()
  , data = formatted_tsdata
  , traj = c("report","death") 
  # remove last element of theta (distr_params) because these get created 
  # internally in calibrator
  # we don't want to fit default states, but they need to be here so we can pass
  # initial values to optimizer
  , par = c(head(theta_names, -1), default_states) 
  , outputs = c("S","E","I","death","report")
  , default = list(
      S = N_focal * (1 - initial_state_prop)
    , R = 0
    , D = 0
  )
)

# save TMB calibrator object so we can work with objective function
focal_tmb = mp_tmb(focal_calib)
length(focal_tmb$par) # notice +2 additional parameters for distr_params
length(theta)+length(default_states)


# objective function of composed model (focal model with initial state vector
# from exp_growth_model)
# exp_growth model depends partially on theta: log_zeta, log_beta0,
# logit_nonhosp_mort. It doesn't depend on log_mobility_coefficients because no
# time-varying transmission in this model. It doesn't depend on log_E because 
# this model gets its own E0 to generate the eigenvector of the linearized model.
# 
# focal model (focal_calib) depends on all of theta
composed_obj_fn = function(theta){
  
    # how to make sure N_focal in exp_growth model is the same as N in focal 
    # model
  
    # only want to return intermediate states (no S, R or D)
    exp_growth_simulator = mp_simulator(exp_growth_model
                                        , time_steps = 100L
                                        , outputs = intermediate_states
                                        , default = list(
                                            log_zeta = theta[1]
                                          , log_beta0 = theta[2]
                                          , logit_nonhosp_mort=theta[3]
                                        )
                                      )
  
    # get final state vector (to become initial state vector for focal model)
    initial_state_vector =  (mp_final(exp_growth_simulator)
                             |> select(matrix, value) 
                             |> pivot_wider(names_from = matrix, values_from=value)
    ) 
    
    state_names = names(initial_state_vector)
    state_values = as.numeric(initial_state_vector[1,])
    
    # update theta with log_E
    # is this a good idea - setting in objective function?
    # we both initialize E and want to fit it, meaning fitted E could create 
    # an initial state vector that is not in eigenvector space?
    #theta[9] =  log(state_values[1]) #log_E
    # theta[10] =  log(state_values[2])
    # theta[11] =  log(state_values[3])
    # theta[12] =  log(state_values[4])
    # theta[13] =  log(state_values[5])

    # negative binomial dispersion parameters for death and reports (we would 
    # want these fit as well ...), these get appended at end of parameter vector
    # I think passing state_values here just initializes these states to these
    # starting values, and because they are not part of theta they don't get
    # optimized.
   focal_tmb$fn(c(theta[1:8], theta[9], state_values[-1], theta[10:11]))
   # focal_tmb$fn(c(theta[1:8], state_values, theta[10:11]))
    
    #focal_tmb$fn(c(theta[1:8], theta[9], theta[10], theta[11], theta[12], theta[13], state_values[6:9], theta[14:15]))
   # focal_tmb$fn(c(theta, state_values))
  
}

#optim(theta, composed_obj_fn, method = "Brent", lower = 0, upper = 2)
composed_obj_fn(theta=theta)

start_time = Sys.time()
testing_optimization = optim(theta, composed_obj_fn ,method="Nelder-Mead")
end_time = Sys.time()
end_time - start_time 
# ~7 minutes

# how would we get fitted data?
# simulate data with "known" parameters (fitted parameters from optim())?
exp_growth_simulator = mp_simulator(exp_growth_model #|> mp_hazard()
                                    , time_steps = 100L
                                    , outputs = intermediate_states
                                    , default = list(
                                        log_zeta =  testing_optimization$par[1]
                                      , log_beta0 = testing_optimization$par[2]
                                      , logit_nonhosp_mort=testing_optimization$par[3]
                                      )
)

initial_state_vector =  (mp_final(exp_growth_simulator)
                         |> select(matrix, value) 
                         |> pivot_wider(names_from = matrix, values_from=value)
) 
fitted_data = (focal_model |> mp_rk4()
               # time steps should match data
               %>% mp_simulator(time_steps = time_steps
                                , outputs = c("foi","incidence","report","death"),
                                # defaults need to be from optimization step
                                # what about distr parameters 
                                # (are these in simulated data?)
                                default = list(
                                    log_zeta = testing_optimization$par[1]
                                  , log_beta0 = testing_optimization$par[2]
                                  , logit_nonhosp_mort = testing_optimization$par[3]
                                  , log_mobility_coefficients = testing_optimization$par[4:8]
                                  #, distr_params = ?
                                  , S = N_focal - exp(testing_optimization$par[9])
                                  #, S = N_focal - initial_state_vector$E
                                  , log_E = testing_optimization$par[9]
                                  #, log_E = log(initial_state_vector$E)
                                  # , log_Ia = testing_optimization$par[10]
                                  # , log_Ip = testing_optimization$par[11]
                                  # , log_Im = testing_optimization$par[12]
                                  # , log_Is = testing_optimization$par[13]
                                  , Ia = initial_state_vector$Ia
                                  , Ip = initial_state_vector$Ip
                                  , Im = initial_state_vector$Im
                                  , Is = initial_state_vector$Is
                                  , H = initial_state_vector$H
                                  , ICUs = initial_state_vector$ICUs
                                  , ICUd = initial_state_vector$ICUd
                                  , H2 = initial_state_vector$H2
                                  , R = 0
                                  , D = 0
                                  )
                                )
               %>% mp_trajectory()
)


(ggplot(formatted_tsdata, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data |> filter(matrix %in% c("death","report"))
              , colour = "red"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)


(ggplot(fitted_data, aes(time,value))
  + geom_point()
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)

##########################################
# old stuff

mp_optimize(focal_calib)

# get fitted data
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)

# check estimate
mp_tmb_coef(focal_calib, conf.int = TRUE) |> backtrans()


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

(ggplot(fitted_data |> filter(matrix %in% c(states)), aes(time,value))
  + geom_point()
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
)


