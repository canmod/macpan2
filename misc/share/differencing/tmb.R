library(macpan2); library(dplyr); library(tidyr); library(ggplot2)

initialize_state = list(S ~ N - I - R)

flows = list(
    mp_per_capita_flow("S", "I", infection ~ I * beta / N)
  , mp_per_capita_flow("I", "R", recovery ~ (1 - f) * gamma)
  , mp_per_capita_flow("I", "D", death ~ f * gamma)
)

default = list(
    beta = 0.2
  , gamma = 0.1
  , N = 100
  , I = 1
  , R = 0
  , D = 0
  , f = 0.1 ## case-fatality rate
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = flows
  , default = default
)


## simulations that assume each time step is a day and 
## compute total number of new deaths each week.

n_days = 101
sundays = seq(from = 1, to = n_days, by = 7) ## assume day 1 is a sunday
sim = (spec
  |> mp_tmb_insert(
        phase = "after"
      , at = Inf
      , expressions = list(
        
          ## get the values for the D compartment
          ## on all of the sundays
          weekly ~ rbind_time(D, sundays)
          
          ## lag-1-week differences
        , weekly ~ block(weekly, 1, 0, n, 1) - block(weekly, 0, 0, n, 1)
      )
      
        ## pass in sunday information as integers
        ## (much less memory intensive)
      , integers = nlist(sundays, n = length(sundays) - 1)
    )
  |> mp_rk4()
  |> mp_simulator(time_steps = n_days, outputs = c("weekly", "death"))
)

daily = mp_trajectory(sim) |> mutate(matrix = "daily")
weekly = mp_final(sim) |> filter(matrix == "weekly") |> mutate(time = sundays[-1])

## looks about right:
mean(weekly$value) / mean(daily$value) ## ~ 7

(rbind(daily, weekly)
  |> ggplot()
  + geom_line(aes(time, value, colour = matrix))
)


