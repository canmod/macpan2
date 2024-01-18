source(system.file("starter_models/si/tmb.R", package = "macpan2"))
library(ggplot2)
library(dplyr)

(specs
  |> lapply(mp_simulator, 50L, "I")
  |> lapply(mp_trajectory)
  |> bind_rows(.id = "integrator")
  |> rename(prevalance = value)
  |> ggplot()
  + geom_line(aes(time, prevalance, colour = integrator))
)
