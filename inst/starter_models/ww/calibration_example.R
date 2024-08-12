library(macpan2)
library(ggplot2)
library(dplyr)
options(macpan2_default_loss = "neg_bin") 

## -------------------------
## Local function to back-transform estimates and CIs
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
## Observed Data Prep
## -------------------------

macpan1.5 = readRDS("misc/experiments/wastewater/macpan1-5_comparison_info.RDS")

# mobility data sourced from ./inst/starter_models/macpan_base/data/get_data.R to describe 
# contact behaviour of individuals
# data is not currently part of package (maybe there was a reason for this, too large etc.)
#mobility_data = readRDS(system.file("inst","starter_models","macpan_base","data","mobility_data.RDS",package="macpan2"))
mobility_data = readRDS(file.path("inst","starter_models","macpan_base","data","mobility_data.RDS"))

# contains identical incidence data to obs_data, with additional variables and longer time frame
prepped_ts_data = readRDS(file.path("inst","starter_models","macpan_base","data","ts_data.RDS"))

# observed incidence and wastewater data
obs_data = (macpan1.5$obs
            |> rename(time = date, matrix = var)
            |> filter(!is.na(value))
            |> mutate(matrix = ifelse(matrix == "report_inc", "reported_incidence", matrix))
)


## controls when the simulations start
starter = data.frame(
    time = as.Date("2020-01-15")
  , matrix = "reported_incidence"
  , value = 0
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

# get number of time steps in observed data
time_steps = nrow(prepped_mobility_data %>% select(date) %>% unique())

# Mobility breakpoints identified in the manuscript for piecewise varying
# transmission.
mobility_breaks = (prepped_mobility_data
                   |> filter(date %in% c("2020-04-01", "2020-08-07"))
                   |> pull(time)
)
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

source("./inst/starter_models/ww/tmb.R")
#specs = mp_tmb_library("starter_models", "ww", alternative_specs = TRUE, package = "macpan2")

# Update model specification to include additional components described here:
# https://github.com/canmod/macpan2/issues/156
focal_model = (#specs$ww_euler
               specs$ww_hazard
               
               # add variable transformations:
               |> mp_tmb_insert(phase = "before"
                                , at = 1L
                                , expressions = list(
                                  #   zeta ~ exp(log_zeta)
                                  # , beta0 ~ exp(log_beta0)
                                  # , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
                                  # , E ~ exp(log_E)
                                  # , Ca ~ exp(log_Ca)
                                  
                                    incidence_report_prob ~ 1 / (1 + exp(-logit_report_prob))
                                  , beta0 ~ exp(log_beta0)
                                  , nu ~ exp(log_nu)
                                )
                                , default = list(
                                  #   log_zeta = empty_matrix
                                  # , log_beta0 = empty_matrix
                                  # , logit_nonhosp_mort = empty_matrix
                                  # , log_E = empty_matrix
                                  # , log_Ca = empty_matrix
                                  
                                   logit_report_prob = 0, log_beta0 = 0, log_nu = 0
                                )
                                
               )
               
               # add accumulator variables:
               # death - new deaths each time step
               |> mp_tmb_insert(phase = "during"
                                , at = Inf
                                , expressions = list(death ~ ICUd.D + Is.D)
               )
               
               # add phenomenological heterogeneity:
               # might need to modify if we add time-varying transmission
               |> mp_tmb_update(phase = "during"
                                , at =1L
                                , expressions = list(mp_per_capita_flow("S", "E", S.E ~ ((S/N)^zeta) * (beta0 / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))
                                
               )
               
               # # compute gamma-density delay kernel for convolution:
               ## |> mp_tmb_insert(phase = "before"
               #                  , at = Inf
               #                  , expressions = list(
               #                      gamma_shape ~ 1 / (c_delay_cv^2)
               #                    , gamma_scale ~ c_delay_mean * c_delay_cv^2
               #                    , gamma ~ pgamma(1:(qmax+1), gamma_shape, gamma_scale)
               #                    , delta ~ gamma[1:(qmax)] - gamma[0:(qmax - 1)]
               #                    , kappa ~ c_prop * delta / sum(delta)
               #                  )
               #                  , default = list(qmax = empty_matrix)
               # )
               # 
               # # add convolution to compute case reports from incidence:
               ## |> mp_tmb_insert(phase = "during"
               #                  , at = Inf
               #                  , expressions = list(reported_incidence ~ convolution(S.E, kappa))
               # )
               
               # add convolution to compute case reports from incidence:
               |> mp_tmb_insert_reports("incidence" 
                                        , report_prob = 0.5
                                        , mean_delay = 11
                                        , cv_delay = 0.25
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
## Data Prep
## -------------------------

# For now, using simulated data to calibrate.

# width of report convolution kernel computed according to:
# https://canmod.net/misc/flex_specs#computing-convolutions
shape = 1/(mp_default_list(focal_model)$c_delay_cv^2)
scale = mp_default_list(focal_model)$c_delay_mean * mp_default_list(focal_model)$c_delay_cv^2
qmax = ceiling(qgamma(0.95, shape, scale))

# simulated data
sim_data = (focal_model 
  |> mp_simulator(
      time_steps = time_steps
    , outputs = c("report","death", "W", "A")
    , default = list(
        S = 14.57e6 - 5 
      , log_E = log(5)
      # infectious states cannot be all zero when using hazard, otherwise incidence
      # at first time step is NaN
      , Ia = 1 
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
      , log_beta0=log(5)
      , logit_nonhosp_mort = -0.5
      , log_zeta = 1
      , qmax = qmax
      , log_Ca = log(2/3)
      , log_mobility_coefficients = rep(0,5)
      )
  )
  |> mp_trajectory()
)

## -------------------------
## calibration
## -------------------------

# The following calibration is only experimental until real time series data
# is used to fit to.
focal_calib = mp_tmb_calibrator(
    spec = focal_model
  , data = rbind(starter, obs_data)
  , traj = c("reported_incidence", "W") 
  , par = c(
      "log_beta0", "log_nu"
    # negative binomial dispersion parameters for reports, deaths, W, and A get added
    # automatically with options(macpan2_default_loss = "neg_bin") set above
  )
  , default = list(
    
    # states
    # Population of Ontario (2019) from:
    # https://github.com/mac-theobio/macpan_base/blob/main/code/ontario_calibrate_comb.R
      S = 14.57e6 - 5 
    #, log_E = log(5)
    # infectious states cannot be all zero when using hazard, otherwise incidence
    # at first time step is NaN
    , Ia = 1
    , Ip = 0
    , Im = 0
    , Is = 0
    , R = 0
    , H = 0
    , ICUs = 0
    , ICUd = 0
    , H2 = 0
    , D = 0
    , W = 0
    , A = 0
    
    , model_matrix_values = model_matrix_values
    
    # set initial parameter values for optimizer
    , log_beta0=log(5)
    #, logit_nonhosp_mort = -0.5
    #, qmax = qmax
    #, log_zeta = 1
    #, log_Ca = log(1/3)
    , log_mobility_coefficients = rep(0,5)
  )
)
# converges
mp_optimize(focal_calib)

# Get fitted data
fitted_data = mp_trajectory_sd(focal_calib, conf.int = TRUE)

# parameter estimates
mp_tmb_coef(focal_calib, conf.int = TRUE) |> backtrans()

# back transform Ca estimate
# estimate is fairly off, perhaps trajectories are not informative enough
# use Ia instead of reports?
# Also, calibration set-up doesn't really make sense, fitting to simulated data
# but using real mobility data to inform the model
(mp_tmb_coef(focal_calib, conf.int = TRUE)[1,]
  |> mutate(mat = "Ca")
  |> mutate_if(is.numeric,exp)
)

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
  )
}

## -------------------------
## previous implementation
## -------------------------

# component = "during"
# identical(
#     specs$explicit$expand()[[component]]
#   , specs$implicit[[component]]
# )
# mp_hazard(specs$explicit)$expand()
# char_expl = sort(vapply(specs$explicit$expand()[[component]], macpan2:::formula_as_character, character(1L)))
# char_impl = sort(vapply(specs$implicit[[component]], macpan2:::formula_as_character, character(1L)))
# setdiff(char_expl, char_impl)
# setdiff(char_impl, char_expl)
# intersect(char_expl, char_impl)
# for (i in 1:33) {
#   print(i)
#   print(char_expl[i])
#   print(char_impl[i])
# }
# char_expl[[34]]
# char_impl[[34]]


## -------------------------
## exploring
## -------------------------
ww_exploring = mp_simulator(
    model = specs$ww_euler
  , time_steps = time_steps
  , outputs = c("Ia", "Ip", "Im", "Is", "W", "A")
) |> mp_trajectory()

## all infectious compartments
if (interactive()) {
  ggplot(ww_exploring %>% filter(grepl("I",matrix))%>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}

## W
if (interactive()) {
  ggplot(ww_exploring %>% filter(matrix=="W")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("W")
}

## A
if (interactive()) {
  ggplot(ww_exploring %>% filter(matrix=="A")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("A")
}
