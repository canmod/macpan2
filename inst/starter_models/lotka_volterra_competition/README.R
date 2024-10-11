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
  , "lotka_volterra_competition"
  , package = "macpan2"
)
print(spec)


## ----fake_data----------------------------------------------------------------
set.seed(1L)
# set number of time steps in simulation
time_steps = 100L
# ayx value to simulate data with (species X has a carrying capacity of 200)
true = list(ayx = 0.8/200)

# simulator object
fake_data = (spec
  |> mp_tmb_insert(
      phase = "during"
    , at = Inf
    , expressions = list(X_noisy ~ rpois(X), Y_noisy ~ rpois(Y))
    , default = true
  )
  |> mp_simulator(  
      time_steps = time_steps
    , outputs = c("X_noisy","Y_noisy")
  )
  |> mp_trajectory()
  |> mutate(matrix = substr(matrix, 1L, 1L))
)

(fake_data
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
  + theme_bw()
)


## -----------------------------------------------------------------------------
cal = (spec
  |> mp_tmb_calibrator(
      data = fake_data
    , traj = c("X", "Y")
    , par = c("ayx","rx", "ry")
  )
)
mp_optimize(cal)


## ----coef---------------------------------------------------------------------
mp_tmb_coef(cal, conf.int = TRUE) |> round_coef_tab()
print(true)


## ----traj_fit-----------------------------------------------------------------
comparison_data = list(
    obs = fake_data
  , fit = mp_trajectory_sd(cal, conf.int = TRUE)
) |> bind_rows(.id = "type")
(comparison_data
  |> ggplot()
  + geom_line(aes(time, value, colour = type))
  + facet_wrap(~matrix)
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , colour = "red"
    , fill = "red"
    , alpha = 0.5
    , filter(comparison_data, type == "fit")
  )
  + theme_bw()
)

