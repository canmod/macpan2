## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.path = "./figures/"
)


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)


## ----model_spec---------------------------------------------------------------
specs = mp_tmb_library(
    "starter_models"
  , "awareness"
  , package = "macpan2"
  , alternative_specs = TRUE
)
print(names(specs))


## ----diagram, echo = FALSE, fig.height = 2, fig.width = 5---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(specs$awareness_model)
plot_flow_diagram(layout)


## ----diagram-delayed-death, echo = FALSE, fig.height = 2, fig.width = 5-------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(specs$delayed_death_awareness_model)
plot_flow_diagram(layout)


## ----echo = FALSE-------------------------------------------------------------
plot_traj = function(traj) {
  p = (traj
    |> mutate(matrix = factor(matrix, levels = outputs))
    |> ggplot()
    + geom_line(aes(time, value))
    + facet_wrap(~matrix
        , scales = "free_y"
        , ncol = 1
        , dir = "v"
      )
    + scale_y_continuous(limits = c(0, NA), expand = c(0, NA))
    + scale_x_continuous(
          minor_breaks = \(x) {
            y = seq(from = x[1], to = x[2], by = 50L)
            y[1:(length(y) - 1)]
          }
        , expand = c(0, 0)
      )
    + ylab("")
    + xlab("")
    + theme_bw()
  )
  return(p)
}


## ----behavioural_cycles-------------------------------------------------------
set.seed(8L)
days = 800
outputs = c(
    "infection", "death"
  , "S", "importation"
)
sim = (specs$longer_memory_awareness_model
  |> mp_euler()
  |> mp_simulator(days, outputs)
)
traj = (sim
  |> mp_trajectory(include_initial = TRUE)
)
plot_traj(traj)


## ----importation--------------------------------------------------------------
set.seed(8L)
days = 800
outputs = c(
    "infection", "death"
  , "S", "importation"
)
sim = (specs$importation_awareness_model
  |> mp_euler()
  |> mp_simulator(days, outputs)
)
traj = (sim
  |> mp_trajectory(include_initial = TRUE)
)
plot_traj(traj)


## ----importation_and_process_error--------------------------------------------
set.seed(8L)
days = 800
outputs = c(
    "infection", "death"
  , "S", "importation"
)
sim = (specs$importation_awareness_model
  |> mp_euler_multinomial()
  |> mp_simulator(days, outputs)
)
traj = (sim
  |> mp_trajectory(include_initial = TRUE)
)
plot_traj(traj)

