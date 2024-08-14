library(macpan2)
library(ggplot2)
library(dplyr)

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

# macpan 1.5 calibration information for wastewater model
macpan1.5 = readRDS("misc/experiments/wastewater/macpan1-5_comparison_info.RDS")

# mobility data sourced from ./inst/starter_models/macpan_base/data/get_data.R to describe 
# contact behaviour of individuals
# data is not currently part of package (maybe there was a reason for this, too large etc.)
# mobility_data = readRDS(system.file("inst","starter_models","macpan_base","data","mobility_data.RDS",package="macpan2"))
mobility_data = readRDS(file.path("inst","starter_models","macpan_base","data","mobility_data.RDS"))

# contains identical incidence data to obs_data, with additional variables and longer time frame
prepped_ts_data = (readRDS(file.path("inst","starter_models","macpan_base","data","ts_data.RDS"))
   # not calibrating to H or ICU for reasons specified in manuscript (https://github.com/mac-theobio/macpan_base/tree/main/outputs)  
   |> filter(var == "death", between(date, macpan1.5$settings_sim$start_date, macpan1.5$settings_sim$end_date))
   |> rename(time = date, matrix = var)
   |> select(-c(province))
)

# observed incidence and wastewater data
obs_data = (macpan1.5$obs
            |> rename(time = date, matrix = var)
            |> mutate(matrix = ifelse(matrix == "report_inc", "reported_incidence", matrix))
            # add death data
            |> union(prepped_ts_data)
            |> filter(!is.na(value))
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
# Alternative transmission change points in macpan1.5$params_tv
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
                                    incidence_report_prob ~ 1 / (1 + exp(-logit_report_prob))
                                  , beta0 ~ exp(log_beta0)
                                  , nu ~ exp(log_nu)
                                  , zeta ~ exp(log_zeta)
                                  , nonhosp_mort ~ 1/(1+exp((-logit_nonhosp_mort)))
                                )
                                , default = list(
                                    logit_report_prob = 0
                                  , log_beta0 = 0
                                  , log_nu = 0
                                  , log_zeta = 0
                                  , logit_nonhosp_mort = empty_matrix
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
                                , at = 1L
                                , expressions = list(mp_per_capita_flow("S", "E", incidence ~ ((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))))

               )
               
               # add convolution to compute case reports from incidence:
               |> mp_tmb_insert_reports("incidence" 
                                        , report_prob = 0.5
                                        , mean_delay = 11
                                        , cv_delay = 0.25
               )
               
               # add time-varying transmission
               |> mp_tmb_insert(phase = "during"
                                , at = 1L
                                , expressions = list(
                                    beta ~ beta0 * beta1
                                )
               )
)

## -------------------------
## calibration
## -------------------------


# get default parameter values
# what about S0, S=S0*N?
useful_params = names(macpan1.5$params) %in% names(focal_model$default)
macpan1.5_defaults = c(
  macpan1.5$params[useful_params]
  , list(S = macpan1.5$params$N
  , log_beta0 = log(macpan1.5$params$beta0)
  , log_nu = log(macpan1.5$params$nu)
  , beta1 = 1
  , E = macpan1.5$params$E0
  , logit_nonhosp_mort = 1
  )
)

# The following calibration is only experimental until real time series data
# is used to fit to.
focal_calib = mp_tmb_calibrator(
    spec = focal_model
  , data = rbind(starter, obs_data)
  , traj = list(
      reported_incidence = mp_neg_bin(disp = 0.1)
    , W = mp_normal(sd = 1)
    #, death = mp_neg_bin(disp = 0.1)
  )
  , par = c(
      "log_beta0", "log_nu", "log_zeta"#, "logit_nonhosp_mort"
  )
  , tv = mp_rbf("beta1", dimension = 5)
  , outputs = c("reported_incidence", "W", "beta", "death")
  # update defaults with macpan1.5 values
  , default = macpan1.5_defaults
)

# converges
mp_optimize(focal_calib)

# Get fitted data
fitted_data = (mp_trajectory_sd(focal_calib, conf.int = TRUE) 
               |> mutate(time = starter$time + lubridate::days(time - 1)
))

# parameter estimates
mp_tmb_coef(focal_calib, conf.int = TRUE) |> backtrans()

# is this from calibration or just simulated from model?
sim1.5 = (macpan1.5$sim
          |> group_by(Date, state)
          |> summarise(value = sum(value))
          |> ungroup()
          |> rename(time = Date, matrix = state)
          |> filter(matrix %in% c("conv", "W"))
          |> filter(!is.na(value))
          |> mutate(matrix = ifelse(matrix == "conv", "reported_incidence", matrix))
)

if (interactive()) {
cols = c("macpan 1.5" = "blue", "macpan 2" = "red")
  (ggplot(obs_data, aes(time,value))
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
   + geom_line(aes(time, value), data = sim1.5, colour = "blue")
   + scale_colour_manual(values = cols)
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
