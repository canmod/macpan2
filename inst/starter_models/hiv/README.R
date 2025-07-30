## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)


## ----packages, warning=FALSE, message=FALSE-----------------------------------
library(macpan2)
library(ggplot2)
library(dplyr)
options(macpan2_verbose = FALSE)


## ----model_lib----------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "hiv"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 2.5, fig.width = 8.5-----------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_grid(spec
  , east = "^(infection|progression|death)"
  , south = "^(protection)"
  , north = "^(unprotection)"
  , loops = "^(unprotection)"
  , x_gap = 0.3
  , y_gap = 0.3
  , north_south_sep = 0.15
)
(layout
  |> plot_flow_diagram(show_flow_rates = TRUE)
  |> draw_outflows(layout, show_labels = TRUE, lab = "rate")
  |> draw_inflows(layout, show_labels = TRUE, lab = "rate")
)


## ----simulations--------------------------------------------------------------
outputs = c(sprintf("I%s", 1:4), sprintf("A%s", 1:4))
sim = (spec
  |> mp_tmb_update(default = list(lambda0 = 0.36, n = 0.2))
  |> mp_rk4()
  |> mp_simulator(time_steps = 50L, outputs)
)
(sim
  |> mp_trajectory()
  |> mutate(matrix = sub("^A([1-4])$", "Infectious and treated, stage \\1", matrix))
  |> mutate(matrix = sub("^I([1-4])$", "Infectious and untreated, stage \\1", matrix))
  |> ggplot()
  + geom_line(aes(time, value))
  + facet_wrap(~ matrix, ncol = 2, scales = 'free', dir = "v")
  + scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
  + theme_bw()
)


## ----simulated_data_for_cal, fig.width = 4------------------------------------
set.seed(1L)
spec_for_cal = (spec
  |> mp_tmb_update(
      default = list(lambda0 = 0.38, n = 0.2)
    , inits = list(
          S = 1e7 - 4000
        , I1 = 1000, I2 = 1000, I3 = 1000, I4 = 1000
        , A1 = 0   , A2 = 0   , A3 = 0   , A4 = 0
      )
  )
  |> mp_rk4()
  |> mp_tmb_insert(at = Inf, expressions = list(
      treated ~ A1 + A2 + A3 + A4
    , untreated ~ I1 + I2 + I3 + I4
  ))
)
simulated_data = (spec_for_cal
  |> mp_simulator(time_steps = 20L, c("treated", "untreated"))
  |> mp_trajectory()
  |> mutate(value = rpois(n(), value))
)
(simulated_data
  |> rename(`Observation Year` = time)
  |> rename(Value = value)
  |> ggplot()
  + geom_line(aes(`Observation Year`, Value))
  + facet_wrap(~matrix, ncol = 1, scales = "free")
  + theme_bw()
)


## ----calibration--------------------------------------------------------------
calibrator = (spec_for_cal
  |> mp_tmb_update(default = list(lambda0 = 0.2, n = 0.5))
  |> mp_tmb_calibrator(
        data = simulated_data
      , traj = list(
            treated = mp_pois()
          , untreated = mp_pois()
      )
      , par = c("log_lambda0", "logit_n")
  )
)
mp_optimize(calibrator)


## ----convergence--------------------------------------------------------------
mp_optimizer_output(calibrator)$convergence


## ----estimates----------------------------------------------------------------
(mp_tmb_coef(calibrator, conf.int = TRUE)
 |> select(-term, -row, -col, -type)
)


## ----fit, fig.width = 4-------------------------------------------------------
(calibrator
 |> mp_trajectory_sd(conf.int = TRUE)
 |> ggplot()
 + geom_line(aes(time, value), colour = "red")
 + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red")
 + geom_line(aes(time, value), data = simulated_data)
 + facet_wrap(~matrix, ncol = 1, scales = "free")
 + theme_bw()
)

