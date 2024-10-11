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

