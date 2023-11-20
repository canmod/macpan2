library(macpan2)
library(dplyr)
library(ggplot2)

source("inst/model_library/lv/model_structure.R")

lv_sim = mp_tmb_simulator(
    dynamic_model = lv
  , vectors = list(
    state = c(X = 10, Y = 10),
    rate = c(
      alpha = 1.1, beta = 0.4, gamma = 0.1, delta = 0.4
    )
  )
  , time_steps = 100L
)

mp_report(lv_sim)