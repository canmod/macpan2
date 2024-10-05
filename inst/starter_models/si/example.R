library(ggplot2)
library(dplyr)

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
)
