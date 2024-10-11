## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)


## ----flow_diagram, echo = FALSE, fig.height=1, fig.width=4, message=FALSE, warning=FALSE----
library(macpan2)
library(ggplot2)
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
si = mp_tmb_library("starter_models", "si", package = "macpan2")
(si 
  |> mp_layout_paths() 
  |> plot_flow_diagram(show_flow_rates = TRUE)
)


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(macpan2)
library(ggplot2)
library(dplyr)


## ----simulation---------------------------------------------------------------
specs = mp_tmb_library("starter_models"
  , "si"
  , package = "macpan2"
  , alternative_specs = TRUE
)
(specs
  |> lapply(mp_simulator, 50L, "I")
  |> lapply(mp_trajectory)
  |> bind_rows(.id = "integrator")
  |> rename(prevalance = value)
  |> ggplot()
  + geom_line(aes(time, prevalance, colour = integrator))
  + theme_bw()
)

