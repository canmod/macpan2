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
  , "sir_waning"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 2, fig.width = 8---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(spec
  , ignore = "waning_immunity"
  , loops = "waning_immunity"
)
p = (layout
  |> plot_flow_diagram()
  + geom_curve(aes(x = xmin_from, y = ymin_from, xend = xmax_to, yend = ymin_from)
    , data = layout$edges_flows_ignored()
    , curvature = -0.2
    , colour = "blue"
    , arrow = mp_flow_arrow
  )
  + ylim(c(0.6, NA))
)
print(p)


## ----simulation---------------------------------------------------------------
set.seed(1L)
time_steps = 100L
true = list(
    beta = 0.3 # beta value to simulate data with
  , phi = 0.05 # phi value to simulate data with
)
sir_waning = (spec
  |> mp_tmb_insert(
      phase = "during", at = Inf
    , expressions = list(noisy_I ~ rpois(I))
    , default = true
  )
  |> mp_simulator(  
      time_steps = time_steps
    , outputs = c("noisy_I", "I")
  )
)
  
observed_data = (sir_waning
  |> mp_trajectory() 
  |> mutate(matrix = ifelse(matrix == "I", "true_I", "I"))
)
(observed_data
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
  + theme_bw()
)


## ----calibration--------------------------------------------------------------
cal = mp_tmb_calibrator(
    spec
  , data = filter(observed_data, matrix == "I")
  , traj = "I"
  , par = c("beta", "phi")
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
  |> filter(matrix == "I")
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

