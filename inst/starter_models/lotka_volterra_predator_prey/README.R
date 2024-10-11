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
specs = mp_tmb_library(
    "starter_models"
  , "lotka_volterra_predator_prey"
  , package = "macpan2"
  , alternative_specs = TRUE
)


## ----simple_spec--------------------------------------------------------------
specs$holling_1


## ----logistic_spec------------------------------------------------------------
specs$holling_2


## ----func_resp_spec-----------------------------------------------------------
specs$holling_3


## ----fake_data----------------------------------------------------------------
spec = specs$holling_1
set.seed(1L)
# set number of time steps in simulation
time_steps = 100L
# delta value to simulate data with
true = list(delta = 2.5/10)

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
  |> mutate(species = ifelse(matrix == "X", "prey", "predator"))
  |> ggplot()
  + geom_line(aes(time, value, colour = species))
  + theme_bw()
)


## -----------------------------------------------------------------------------
cal = (spec
  |> mp_tmb_calibrator(
      data = fake_data
    , traj = c("X", "Y")
    , par = "delta"
  )
)
mp_optimize(cal)


## ----coef---------------------------------------------------------------------
coef = mp_tmb_coef(cal) |> round_coef_tab()
coef$true = true[coef$mat]
print(coef)


## ----traj_fit-----------------------------------------------------------------
comparison_data = list(
    obs = fake_data
  , fit = mp_trajectory_sd(cal, conf.int = TRUE)
) |> bind_rows(.id = "type") |> mutate(species = ifelse(matrix == "X", "prey", "predator"))
(comparison_data
  |> ggplot()
  + geom_line(aes(time, value, colour = type))
  + facet_wrap(~species)
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
    , colour = "red"
    , fill = "red"
    , alpha = 0.5
    , filter(comparison_data, type == "fit")
  )
  + theme_bw()
)

