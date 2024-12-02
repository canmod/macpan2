## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)
round_coef_tab = function(x, digits = 4) {
  id_cols = c("term", "mat", "row", "col", "type")
  num_cols = setdiff(names(x), id_cols)
  for (col in num_cols) {
    x[[col]] = round(x[[col]], digits)
  }
  rownames(x) = NULL
  cols_to_drop = c("term", "col", "type")
  x = x[, setdiff(names(x), cols_to_drop), drop = FALSE]
  x
}


## ----packages, warning=FALSE, message=FALSE-----------------------------------
library(macpan2)
library(ggplot2)
library(dplyr)
library(lubridate)



## ----options------------------------------------------------------------------
options(macpan2_verbose = FALSE)


## ----model_lib----------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "ww"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 4, fig.width = 8.5-------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout_base = mp_layout_paths(spec, x_gap = 0.1, y_gap = 0.1, trim_states = c("W", "A"))
layout_ww = mp_layout_paths(spec, x_gap = 0.1, y_gap = 0.1, trim_states = c("A", setdiff(layout_base$states(), c("Ia", "Ip", "Im", "Is"))))
plot_flow_diagram(layout_base)


## ----diagram_shedding, echo = FALSE, fig.height = 2, fig.width = 3------------
plot_flow_diagram(layout_ww)


## ----default_load-------------------------------------------------------------
repo = "https://github.com/canmod/macpan2/releases/download/macpan1.5_data"
macpan1.5 = (repo
  |> file.path("covid_on_macpan1.5_calibration.RDS")
  |> url() 
  |> readRDS()
)


## ----covid_data---------------------------------------------------------------
covid_on = (repo
  |> file.path("covid_on.RDS")
  |> url() 
  |> readRDS()
)


## ----prep---------------------------------------------------------------------
obs_data = (covid_on
    |> filter(var %in% c("report","W") & between(date,macpan1.5$settings_sim$start_date,macpan1.5$settings_sim$end_date))
    |> rename(time = date, matrix = var)
    |> mutate(matrix = ifelse(matrix == "report", "reported_incidence", matrix))
    # leading zeroes seem to cause calibration challenges
    |> filter(!is.na(value) & time > as.Date("2020-02-23"))
)


## ----time_bounds--------------------------------------------------------------
burn_in_period  = 15 ## number of days before the data start to begin the simulations
forecast_period = 30 ## number of days after the data end to make forecasts
time_bounds = mp_sim_offset(
    sim_start_offset = 15
  , sim_end_offset = 30
  , time_scale = "daily"
)


## ----cal_spec_update----------------------------------------------------------
focal_model = (
  # waste water model with hazard correction
  spec |> mp_hazard()
     
  # add variable transformations:
  |> mp_tmb_insert(phase = "before"
     , at = 1L
     , expressions = list(
          beta0 ~ exp(log_beta0)
        , nu ~ exp(log_nu)
        , xi ~ exp(log_xi)
        , report_prob ~ 1 / (1 + exp(-logit_report_prob))
     )
     , default = list(
          log_beta0 = log(1)
        , log_nu = 0
        , log_xi = 0
        , logit_report_prob = qlogis(0.1)
     )
  )

  # add accumulator variables:
  # death - new deaths each time step
  |> mp_tmb_insert(phase = "during"
     , at = Inf
     , expressions = list(
          death ~ ICUd.D + Is.D
       ,  prevalence ~ Ip + Ia + Im + Is
      )
  )
     
  # add phenomenological heterogeneity:
  |> mp_tmb_update(phase = "during"
     , at = 1L
     , expressions = list(mp_per_capita_flow("S", "E", "((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))", "incidence"))

  )
     
  # add convolution to compute case reports from incidence:
  |> mp_tmb_insert_reports("incidence"
     , report_prob = NA_real_  # defined above with a logit transform
     , mean_delay = 11
     , cv_delay = 0.25
     , report_prob_name = "report_prob"
  )

  # add time-varying transmission
  |> mp_tmb_insert(phase = "during"
     , at = 1L
     , expressions = list(beta ~ beta0 * beta1 * beta2)
  )
  
  |> mp_tmb_insert(phase = "during"
     , at = 1L
     , expressions = list(
          beta2 ~ time_var(beta_changes, beta_changepoints)
     )
     , default  = list(beta_changes      = c(1))#, 3))
     , integers = list(beta_changepoints = c(0))#, steps("2021-03-05")))
  )
)


## -----------------------------------------------------------------------------
useful_params = names(macpan1.5$params) %in% names(focal_model$default)
macpan1.5_defaults = c(
    macpan1.5$params[useful_params]
  , list(
      S = macpan1.5$params$N
    , log_beta0 = log(macpan1.5$params$beta0)
    , log_nu = log(macpan1.5$params$nu)
    , beta1 = 1
    , E = macpan1.5$params$E0
  )
)

# calibrator
focal_calib = mp_tmb_calibrator(
    spec = focal_model
  , data = obs_data
  , traj = list(
      # set likelihoods for trajectories we are fitting to
      reported_incidence = mp_neg_bin(disp = 0.1)
    , W = mp_normal(sd = 1)
  )
  , par = c(
        # parameters to fit
        "log_beta0" # baseline transmission
      , "log_nu"    # viral shedding rate
      , "log_xi"
      #, "logit_report_prob" # reporting probability (only makes sense to fit if you fit to incidence)
  )
    # flexible non-parametric (radial basis function) fit
    # for the time-varying transmission rate
  , tv = mp_rbf("beta1", dimension = 7, sparse_tol = 1e-4)
    
    # return these trajectories so that they can be
    # explored after fitting
  , outputs = c(
        "reported_incidence", "W", "beta"
      , "prevalence", "incidence", "S"
    )
  
    # update defaults with macpan1.5 wastewater model defaults
  , default = macpan1.5_defaults
  
  , time = time_bounds
)
# converges
mp_optimize(focal_calib)


## ----get_fits-----------------------------------------------------------------
macpan2_fit = mp_trajectory_sd(focal_calib, conf.int = TRUE)
fitted_coefs = mp_tmb_coef(focal_calib, conf.int = TRUE)
print(fitted_coefs)


## ----plot_fits----------------------------------------------------------------
macpan1.5_fit = (macpan1.5$sim
  |> group_by(Date, state)
  # summarize to ignore vaccination status
  |> summarise(value = sum(value))
  |> ungroup()
  |> rename(time = Date, matrix = state)
  |> filter(matrix %in% c("conv", "W"))
  |> filter(!is.na(value))
  |> mutate(matrix = ifelse(matrix == "conv", "reported_incidence", matrix))
)

plot_fit = function(obs_data, sim_data, ncol = 1L) {
  (ggplot(obs_data, aes(time, value))
   + geom_point()
   + geom_line(aes(
          time
        , value
        , colour = source
      )
      , data = sim_data
   )
   #+ scale_y_log10()
   + geom_ribbon(aes(
          time
        , ymin = conf.low
        , ymax = conf.high
        , colour = source
        , fill = source
      )
      , data = sim_data
      , alpha = 0.2
   )
   + facet_wrap(vars(matrix), scales = 'free', ncol = ncol)
   + theme_bw()
   + theme(legend.position = "top")
   #+ scale_x_date(limits = c(as.Date("2020-11-01"), NA))
  )
}
plot_fit(obs_data, mutate(macpan2_fit, source = "macpan2_fit"), ncol = 2L)

