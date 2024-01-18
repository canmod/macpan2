library(macpan2)
library(dplyr)
library(ggplot2)

## complexities
## 
## 1. time-varying parameters
## 2. reporting delays and under-reporting
## 3. hazard correction

initializer = list(I ~ 1, S ~ N - I)
flow_rates = list(
  infection ~ beta * S * I / N
)
state_updates = list(
    S ~ S - infection
  , I ~ I + infection
)
default = list(N = 100, beta = 0.25)

si = mp_tmb_model_spec(
    before = initializer
  , during = c(flow_rates, state_updates)
  , default = default
)
si |> mp_simulator(50, "infection") |> mp_trajectory()


## -----------------------
## reporting delays and under-reporting
## -----------------------

## https://canmod.net/misc/flex_specs#computing-convolutions
reporting = list(
  reports ~ 0.1 * convolution(infection, c(0.25, 0.25, 0.5))
)
si_conv = mp_tmb_model_spec(
    before = initializer
  , during = c(flow_rates, state_updates, reporting)
  , default = default
)
(si_conv 
  |> mp_simulator(50, c("infection", "reports")) 
  |> mp_trajectory()
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
)



## -----------------------
## time-varying parameters
## -----------------------

strain_takeover = list(
    p ~ p0 / (p0 + (1 - p0) * exp(-delta_r * time_step(0)))
  , beta ~ beta * (advantage * p + (1 - p))
)
strain_default = list(
    p0 = 1 / 20
  , advantage = 1.1
  , delta_r = 0.1
)
si_strain = mp_tmb_model_spec(
    before = initializer
  , during = c(strain_takeover, flow_rates, state_updates, reporting)
  , default = c(default, strain_default)
)
(si_strain 
  |> mp_simulator(50, c("p", "infection")) 
  |> mp_trajectory()
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
)



## step-function version

lockdown = list(
    j ~ time_group(j, change_points)
  , beta ~ beta_t[j]
)
lockdown_default = list(
    j = 0
  , change_points = c(0, 10, 30)
  , beta_t = c(0.5, 0.2, 0.5)
)
si_lockdown = mp_tmb_model_spec(
    before = initializer
  , during = c(lockdown, flow_rates, state_updates, reporting)
  , default = c(default, lockdown_default)
)
(si_lockdown 
  |> mp_simulator(50, c("infection")) 
  |> mp_trajectory()
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
)
