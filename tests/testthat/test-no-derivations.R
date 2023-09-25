library(macpan2)
library(dplyr)
library(ggplot2)
library(oor)
age = Compartmental(system.file("starter_models", "age", package = "macpan2"))
s = age$simulators$tmb(time_steps = 100L
  , state = c(age_0_19 = 100, age_20_39 = 0, age_40_59 = 0, age_60_79 = 0, age_80_99 = 0, age_100_plus = 0)
  , flow = c(
    birth_rate_0_19 = 0, birth_rate_20_39 = 0.09, birth_rate_40_59 = 0.02,
    birth_rate_60_79 = 0, birth_rate_80_99 = 0, birth_rate_100_plus = 0,
    death_rate_0_19 = 0.01, death_rate_20_39 = 0.03, death_rate_40_59 = 0.05,
    death_rate_60_79 = 0.1, death_rate_80_99 = 0.2, death_rate_100_plus = 0.5,
    age_rate = 1 / 20
  )
)
(s$report()
  |> filter(matrix == "state", row != "dead")
  |> ggplot()
  + geom_line(aes(time, value, colour = row))
)
