## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.path = "./figures/"
)
system.file("utils", "round-coef.R", package = "macpan2") |> source()


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)


## ----options------------------------------------------------------------------
options(macpan2_verbose = FALSE)


## ----model_spec---------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "seir"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 1, fig.width = 7---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(spec)
p = plot_flow_diagram(layout, show_flow_rates = TRUE)
print(p)


## ----simulation---------------------------------------------------------------
set.seed(1L)
time_steps = 100L
true = list(
    beta = 0.3 # beta value to simulate data with
  , gamma = 0.05 # gamma value to simulate data with
  , alpha = 0.1 # alpha value to simulate data with
)
seir = (spec
  |> mp_tmb_insert(
      phase = "during", at = Inf
    , expressions = list(noisy_infection ~ rpois(infection))
    , default = true
  )
  |> mp_simulator(
      time_steps = time_steps
    , outputs = c("noisy_infection", "infection")
  )
)
  
observed_data = (seir
  |> mp_trajectory() 
  |> mutate(matrix = ifelse(matrix == "infection", "true_infection", "infection"))
)
(observed_data
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
  + theme_bw()
)


## -----------------------------------------------------------------------------
print(spec |> mp_expand())


## ----calibration--------------------------------------------------------------
cal = mp_tmb_calibrator(
    spec
  , data = filter(observed_data, matrix == "infection")
  , traj = "infection"
  , par = c("beta", "gamma", "alpha")
)
mp_optimize(cal)


## ----coef---------------------------------------------------------------------
coef = mp_tmb_coef(cal) |> round_coef_tab()
coef$true = true[coef$mat]
print(coef)


## ----traj_fit-----------------------------------------------------------------
comparison_data = list(
    obs = observed_data
  , fit = mp_trajectory_sd(cal, conf.int = TRUE)
) |> bind_rows(.id = "type")
(comparison_data
  |> filter(matrix == "infection")
  |> ggplot()
  + geom_line(aes(time, value, colour = type))
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , colour = "red"
    , fill = "red"
    , alpha = 0.5
    , filter(comparison_data, type == "fit")
  )
  + theme_bw()
)

