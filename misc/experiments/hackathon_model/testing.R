library(macpan2)

model_dir = "misc/experiments/hackathon_model/"
hack_model = Compartmental(model_dir)
hack_simulator = hack_model$simulators$tmb(
  time_steps = 50,
  state = c(S.lb0 = 1000000, E.lb0 = 1, I.lb0 = 1, H.lb0 = 0, R.lb0 = 0, D.lb0 = 0,
            S.lb5 = 1000000, E.lb5 = 1, I.lb5 = 1, H.lb5 = 0, R.lb5 = 0, D.lb5 = 0,
            S.lb10 = 1000000, E.lb10 = 1, I.lb10 = 1, H.lb10 = 0, R.lb10 = 0, D.lb10 = 0,
            S.lb15 = 1000000, E.lb15 = 1, I.lb15 = 1, H.lb15 = 0, R.lb15 = 0, D.lb15 = 0,
            S.lb20 = 1000000, E.lb20 = 1, I.lb20 = 1, H.lb20 = 0, R.lb20 = 0, D.lb20 = 0,
            S.lb25 = 1000000, E.lb25 = 1, I.lb25 = 1, H.lb25 = 0, R.lb25 = 0, D.lb25 = 0,
            S.lb30 = 1000000, E.lb30 = 1, I.lb30 = 1, H.lb30 = 0, R.lb30 = 0, D.lb30 = 0,
            S.lb35 = 1000000, E.lb35 = 1, I.lb35 = 1, H.lb35 = 0, R.lb35 = 0, D.lb35 = 0,
            S.lb40 = 1000000, E.lb40 = 1, I.lb40 = 1, H.lb40 = 0, R.lb40 = 0, D.lb40 = 0, 
            S.lb45 = 1000000, E.lb45 = 1, I.lb45 = 1, H.lb45 = 0, R.lb45 = 0, D.lb45 = 0,
            S.lb50 = 1000000, E.lb50 = 1, I.lb50 = 1, H.lb50 = 0, R.lb50 = 0, D.lb50 = 0,
            S.lb55 = 1000000, E.lb55 = 1, I.lb55 = 1, H.lb55 = 0, R.lb55 = 0, D.lb55 = 0,
            S.lb60 = 1000000, E.lb60 = 1, I.lb60 = 1, H.lb60 = 0, R.lb60 = 0, D.lb60 = 0,
            S.lb65 = 1000000, E.lb65 = 1, I.lb65 = 1, H.lb65 = 0, R.lb65 = 0, D.lb65 = 0,
            S.lb70 = 1000000, E.lb70 = 1, I.lb70 = 1, H.lb70 = 0, R.lb70 = 0, D.lb70 = 0,
            S.lb75 = 1000000, E.lb75 = 1, I.lb75 = 1, H.lb75 = 0, R.lb75 = 0, D.lb75 = 0,
            S.lb80 = 1000000, E.lb80 = 1, I.lb80 = 1, H.lb80 = 0, R.lb80 = 0, D.lb80 = 0),
  flow = c(infection.lb0  = 0.2, progression.lb0 = 0.2, hospitalization.lb0 = 0.1, recovery.lb0 = 0.1, discharge.lb0 = 0.1, death_H.lb0 = 0.01, death_I.lb0 = 0.01
          , infection.lb5 = 0.2, progression.lb5 = 0.2, hospitalization.lb5 = 0.1, recovery.lb5 = 0.1, discharge.lb5 = 0.1, death_H.lb5 = 0.01, death_I.lb5 = 0.01
          , infection.lb10 = 0.2, progression.lb10 = 0.2, hospitalization.lb10 = 0.1, recovery.lb10 = 0.1, discharge.lb10 = 0.1, death_H.lb10 = 0.01, death_I.lb10 = 0.01
          , infection.lb15 = 0.2, progression.lb15 = 0.2, hospitalization.lb15 = 0.1, recovery.lb15 = 0.1, discharge.lb15 = 0.1, death_H.lb15 = 0.01, death_I.lb15 = 0.01
          , infection.lb20 = 0.2, progression.lb20 = 0.2, hospitalization.lb20 = 0.1, recovery.lb20 = 0.1, discharge.lb20 = 0.1, death_H.lb20 = 0.01, death_I.lb20 = 0.01
          , infection.lb25 = 0.2, progression.lb25 = 0.2, hospitalization.lb25 = 0.1, recovery.lb25 = 0.1, discharge.lb25 = 0.1, death_H.lb25 = 0.01, death_I.lb25 = 0.01
          , infection.lb30 = 0.2, progression.lb30 = 0.2, hospitalization.lb30 = 0.1, recovery.lb30 = 0.1, discharge.lb30 = 0.1, death_H.lb30 = 0.01, death_I.lb30 = 0.01
          , infection.lb35 = 0.2, progression.lb35 = 0.2, hospitalization.lb35 = 0.1, recovery.lb35 = 0.1, discharge.lb35 = 0.1, death_H.lb35 = 0.01, death_I.lb35 = 0.01
          , infection.lb40 = 0.2, progression.lb40 = 0.2, hospitalization.lb40 = 0.1, recovery.lb40 = 0.1, discharge.lb40 = 0.1, death_H.lb40 = 0.01, death_I.lb40 = 0.01
          , infection.lb45 = 0.2, progression.lb45 = 0.2, hospitalization.lb45 = 0.1, recovery.lb45 = 0.1, discharge.lb45 = 0.1, death_H.lb45 = 0.01, death_I.lb45 = 0.01
          , infection.lb50 = 0.2, progression.lb50 = 0.2, hospitalization.lb50 = 0.1, recovery.lb50 = 0.1, discharge.lb50 = 0.1, death_H.lb50 = 0.01, death_I.lb50 = 0.01
          , infection.lb55 = 0.2, progression.lb55 = 0.2, hospitalization.lb55 = 0.1, recovery.lb55 = 0.1, discharge.lb55 = 0.1, death_H.lb55 = 0.01, death_I.lb55 = 0.01
          , infection.lb60 = 0.2, progression.lb60 = 0.2, hospitalization.lb60 = 0.1, recovery.lb60 = 0.1, discharge.lb60 = 0.1, death_H.lb60 = 0.01, death_I.lb60 = 0.01
          , infection.lb65 = 0.2, progression.lb65 = 0.2, hospitalization.lb65 = 0.1, recovery.lb65 = 0.1, discharge.lb65 = 0.1, death_H.lb65 = 0.01, death_I.lb65 = 0.01
          , infection.lb70 = 0.2, progression.lb70 = 0.2, hospitalization.lb70 = 0.1, recovery.lb70 = 0.1, discharge.lb70 = 0.1, death_H.lb70 = 0.01, death_I.lb70 = 0.01
          , infection.lb75 = 0.2, progression.lb75 = 0.2, hospitalization.lb75 = 0.1, recovery.lb75 = 0.1, discharge.lb75 = 0.1, death_H.lb75 = 0.01, death_I.lb75 = 0.01
          , infection.lb80 = 0.2, progression.lb80 = 0.2, hospitalization.lb80 = 0.1, recovery.lb80 = 0.1, discharge.lb80 = 0.1, death_H.lb80 = 0.01, death_I.lb80 = 0.01),
  transmission. = rep(1.5, times = 17),
  contact. = matrix(data = rep(0.05882353, times = 289), nrow = 17, ncol = 17),
  N.lb0 = empty_matrix,
  N.lb5 = empty_matrix,
  N.lb10 = empty_matrix,
  N.lb15 = empty_matrix,
  N.lb20 = empty_matrix,
  N.lb25 = empty_matrix,
  N.lb30 = empty_matrix,
  N.lb35 = empty_matrix,
  N.lb40 = empty_matrix,
  N.lb45 = empty_matrix,
  N.lb50 = empty_matrix,
  N.lb55 = empty_matrix,
  N.lb60 = empty_matrix,
  N.lb65 = empty_matrix,
  N.lb70 = empty_matrix,
  N.lb75 = empty_matrix,
  N.lb80 = empty_matrix,
  N. = empty_matrix,
  scaled_infected. = empty_matrix,
  infection. = empty_matrix,
  infected. = empty_matrix,
  dummy. = empty_matrix
)

hack_simulator$report()
