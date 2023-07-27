library(macpan2)
m = Compartmental("misc/experiments/hackathon_simplified_model/"
)$simulators$tmb(
  time_steps = 50,
    state = c(S.young = 1000000, E.young = 1, R.young = 1, H.young = 0, I.young = 0, D.young = 0,
              S.old = 1000000, E.old = 1, R.old = 1, H.old = 0, I.old = 0, D.old = 0),
  # "S.young", "E.young", "R.young", "H.young", "I.young", "D.young",
  #   "S.old", "E.old", "R.old", "H.old", "I.old", "D.old"
    flow = c(infection.young  = 0.2, progression.young = 0.2, hospitalization.young = 0.1, recovery.young = 0.1, discharge.young = 0.1, death_H.young = 0.01, death_I.young = 0.01
             , infection.old = 0.2, progression.old = 0.2, hospitalization.old = 0.1, recovery.old = 0.1, discharge.old = 0.1, death_H.old = 0.01, death_I.old = 0.01),
    transmission.young = 1.5,
    transmission.old = 1.5,
    transmission. = empty_matrix,
    contact. = matrix(data = rep(0.25, times = 4), nrow = 2, ncol = 2),
    N.young = empty_matrix,
    N.old = empty_matrix,
    N. = empty_matrix,
    scaled_infected. = empty_matrix,
    infection. = empty_matrix,
    infected. = empty_matrix,
    dummy. = empty_matrix
)

