## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "./figures/"
)
round_coef_tab = function(x, digits = 4) {
  id_cols = c("term", "mat", "row", "col", "type")
  num_cols = setdiff(names(x), id_cols)
  for (col in num_cols) {
    x[[col]] = round(x[[col]], digits)
  }
  rownames(x) = NULL
  cols_to_drop = c("term", "col", "type")
  x = x[, setdiff(names(x), cols_to_drop), drop = FALSE]
  x
}
system.file("utils", "box-drawing.R", package = "macpan2") |> source()


## ----packages, warning=FALSE, message=FALSE-----------------------------------
library(macpan2)
library(ggplot2)
library(dplyr)


## ----model_lib----------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "macpan_base"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 5, fig.width = 8---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(spec, x_gap = 0.1, y_gap = 0.1)
plot_flow_diagram(layout)

