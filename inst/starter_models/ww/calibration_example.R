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

# set a starting point for the simulation (earlier than observed data)
starter = data.frame(
    time = as.Date("2020-01-15")
  , matrix = "reported_incidence"
  , value = 0
)

# observed incidence and wastewater data to calibrate to
obs_data = (macpan1.5$obs
            |> rename(time = date, matrix = var)
            |> mutate(matrix = ifelse(matrix == "report_inc", "reported_incidence", matrix))
            |> filter(!is.na(value))
            |> bind_rows(starter)
)

## -------------------------
## Model Specification
## -------------------------

# Get model spec from the library
specs = mp_tmb_library("starter_models", "ww", alternative_specs = TRUE, package = "macpan2")

# update model specification to include additional components
focal_model = (
  # waste water model with hazard correction
  specs$ww_hazard
     
  # add variable transformations:
  |> mp_tmb_insert(phase = "before"
                   , at = 1L
                   , expressions = list(
                        incidence_report_prob ~ 1 / (1 + exp(-logit_report_prob))
                      , beta0 ~ exp(log_beta0)
                      , nu ~ exp(log_nu)
                      , zeta ~ exp(log_zeta)
                   )
                   , default = list(
                        logit_report_prob = 0
                      , log_beta0 = 0
                      , log_nu = 0
                      , log_zeta = 0
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
                   , expressions = list(mp_per_capita_flow("S", "E", "((S/N)^zeta) * (beta / N) * (Ia * Ca + Ip * Cp + Im * Cm * (1 - iso_m) + Is * Cs *(1 - iso_s))", "incidence"))

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
                   , expressions = list(beta ~ beta0 * beta1)
  )

)

## -------------------------
## calibration
## -------------------------

# get default parameter values from macpan 1.5
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
      # set priors for trajectories we are fitting to
      reported_incidence = mp_neg_bin(disp = 0.1)
    , W = mp_normal(sd = 1)
  )
  , par = c(
      "log_beta0", "log_nu" , "log_zeta"
  )
  # use radial basis functions for beta1
  , tv = mp_rbf("beta1", dimension = 5)
  , outputs = c("reported_incidence", "W", "beta")
  # update defaults with macpan1.5 defaults
  , default = macpan1.5_defaults
)

# converges
replicate(15, mp_optimize(focal_calib), simplify = FALSE)

# get fitted data
macpan2_fit = (mp_trajectory_sd(focal_calib, conf.int = TRUE) 
   |> mutate(time = starter$time + lubridate::days(time - 1))
)

# parameter estimates, back-transformed to be on original scale
# phenomenological heterogeneity parameter zeta ~ 0(wide CI), might not be necessary to include in the model
mp_tmb_coef(focal_calib, conf.int = TRUE) |> backtrans()

# fitted data from macpan 1.5
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

if (interactive()) {
  
# calibration comparison between macpan 1.5 and macpan 2
all_data = bind_rows(lst(macpan1.5_fit, macpan2_fit), .id = "source")

  (ggplot(obs_data, aes(time,value))
   + geom_point()
   + geom_line(aes(time, value, colour = source)
               , data = all_data
   )
   + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high, colour = source, fill = source)
                 , data = all_data
                 , alpha = 0.2
   )
   + facet_wrap(vars(matrix),scales = 'free')
   + theme_bw()
  )

}

## -------------------------
## exploring
## -------------------------
ww_exploring = mp_simulator(
    model = specs$ww_euler
  , time_steps = 100
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
