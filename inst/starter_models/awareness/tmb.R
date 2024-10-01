library(macpan2)

params = list(
  ## probability that one more infectious individual 
  ## will arrive on any day.
    importation_prob = 1/100
  
  ## how long do people 'care about' deaths (days)
  , memory_length = 50
  
  ## mean time between isolation and death (days)
  , death_delay = 25
  
  ## half-saturation constant for death awareness.  
  , delta_c = 1/100
  
  ## half-saturation constant for longer-term death
  ## awareness.  
  , delta_c_long = 10
)

population = 1e5
awareness_model = mp_tmb_model_spec(
    before = list(S ~ N - I, gamma_r ~ (1 - f_D) * gamma, gamma_d ~ f_D * gamma)
  , during = list(
        N ~ S + E + I + R
      , mp_per_capita_flow("I", "D", "gamma_d", "death")
      , mp_per_capita_flow("S", "E", "beta * I / (1 + (death / delta_c)^k) / N", "infection")
      , mp_per_capita_flow("E", "I", "mu", "progression")
      , mp_per_capita_flow("I", "R", "gamma_r", "recovery")
    )
  , default = list(
        N = population, E = 0, I = 1, R = 0, D = 0
      , beta = 1/2
      , mu = 1/2
      , gamma = 1/6
      , f_D = 0.01
      , delta_c = params$delta_c
      , k = 2
  )
)

delayed_death_awareness_model = (awareness_model
  |> mp_tmb_update(
      at = 1L
    , expressions = list(N ~ S + E + I + R + H)
    , default = list(H = 0)
  )
  |> mp_tmb_update(
      at = 2L
    , expressions = list(mp_per_capita_flow("H", "D", "gamma_h", "death"))
    , default = list(gamma_h = 1/params$death_delay)
  )
  |> mp_tmb_insert(
      at = 2L
    , expressions = list(mp_per_capita_flow("I", "H", "gamma_d", "hospitalization"))
  )
)

longer_memory_awareness_model = (delayed_death_awareness_model
  |> mp_tmb_update(at = 4
      , expressions = list(
        mp_per_capita_flow(
            from = "S"
          , to = "E"
          , rate = "beta * I / (1 + (convolution(death, kernel)/delta_c)^k) / N"
          , abs_rate = "infection"
        )
      )
     , default = list(
         delta_c = params$delta_c_long
       , kernel = rep(1, params$memory_length)
      )
    )
)


importation_awareness_model = (longer_memory_awareness_model
  |> mp_tmb_insert(
        expressions = list(
            importation ~ rbinom(1, importation_prob)
          , I ~ I + importation
          , R ~ R - importation
        )
      , default = list(
            I = 0
          , importation_prob = params$importation_prob
        )
  )
)

specs = nlist(
    awareness_model
  , delayed_death_awareness_model
  , longer_memory_awareness_model
  , importation_awareness_model
)

spec = specs$awareness_model
