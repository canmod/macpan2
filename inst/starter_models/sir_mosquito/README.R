## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(macpan2)
library(ggplot2)


## ----library------------------------------------------------------------------
spec = mp_tmb_library("starter_models", "sir_mosquito", package = "macpan2")


## ----diagram, echo = FALSE, fig.height = 3, fig.width = 8.5-------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
repl = "^([a-z]*)(_.*)" ## remove after unscore of flow names
layout = mp_layout_paths(spec)
(layout
  |> plot_flow_diagram(show_flow_names = TRUE
    , pattern_edge_mutate = repl
    , pattern_edge_replace = "\\1"
    , state_dependence = mp_state_dependence_frame(spec)
  ) 
  |> draw_outflows(layout, show_labels = TRUE
    , x = "xmax", y = "y"
    , x_dir = "east", y_dir = "central"
    , pattern_filter = "^recovery"
    , pattern_mutate = repl
    , pattern_replace = "\\1"
  )
  |> draw_outflows(layout, show_labels = TRUE
    , y = "ymax", y_dir = "north"
    , pattern_filter = "^death_(S|I)_H"
    , pattern_mutate = repl
    , pattern_replace = "\\1"
  )
  |> draw_outflows(layout, show_labels = TRUE
    , y = "ymin", y_dir = "south"
    , pattern_filter = "^death_(S|I)_M"
    , pattern_mutate = repl
    , pattern_replace = "\\1"
  )
  |> draw_inflows(layout, show_labels = TRUE
    , pattern_mutate = repl
    , pattern_replace = "\\1"
  )
)


## ----simulations--------------------------------------------------------------
(spec
  |> mp_rk4()
  |> mp_simulator(time_steps = 500, outputs = c("S_H", "I_H", "S_M", "I_M"))
  |> mp_trajectory(include_initial = TRUE)
  |> ggplot()
  + facet_wrap(~matrix, scales = "free")
  + geom_line(aes(time, value))
  + theme_bw()
)

