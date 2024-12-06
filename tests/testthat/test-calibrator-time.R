test_that("times can be supplied as times, and assume daily time-step", {
  sir_obs = (read.csv(
    system.file("starter_models/shiver/data/hospitalizations_ontario.csv", package = "macpan2")
    , row.names = NULL
    ) 
    |>  dplyr::rename(time = date)
    |>  dplyr::group_by(time)
    |>  dplyr::summarize(value = sum(icu_unvac, icu_partial_vac, icu_full_vac,
                            hospitalnonicu_unvac, hospitalnonicu_partial_vac, 
                            hospitalnonicu_full_vac))
    |>  dplyr::ungroup()
    |>  dplyr::mutate(matrix = "I") # not actually I, just using as an example
  )
  
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  
    sir_calib  = mp_tmb_calibrator(sir
     , data = sir_obs
     , traj = "I"
     , par = "beta"
  )
  
  expect_identical(sir_calib$cal_spec$integers$obs_times_I, 1:318)
  expect_identical(sir_calib$time_steps_obj$external_to_internal(mp_trajectory(sir_calib)$time), 1:318)
  expect_identical(mp_trajectory(sir_calib)$time, sir_calib$time_steps_obj$internal_to_external(1:318))
})

test_that("start-date offsets work", {
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  offset = 30
  sim = (sir
    |> mp_simulator(100, "I") 
    |> mp_trajectory() 
    
    ## start _data_ late to test offsets
    |> filter(time > offset) 
    |> mutate(time = seq_along(time) - 1L)
  )
  cal = mp_tmb_calibrator(sir
    , data = sim
    , traj = "I"
    , par = "beta"
    
    ## start _simulations_ early to test offsets
    , time = mp_sim_offset(30, 0, "steps")
    
    ## start optimization with beta away from the true value
    , default = list(beta = 0.9)
  )
  mp_optimize(cal)
  fitted_data = mp_trajectory(cal)
  (sim
    |> ggplot()
    + geom_point(aes(time, value))
    + geom_line(aes(time, value), data = fitted_data)
    + theme_bw()
  )
  cc = mp_tmb_coef(cal)
  expect_identical(round(cc$estimate, 5), 0.2)
  expect_identical(round(cc$std.error, 4), 0.003)
  
  sim_dates = mutate(sim, time = as.Date("2016-07-08") + time)
  cal_dates = mp_tmb_calibrator(sir
    , data = sim_dates
    , traj = "I"
    , par = "beta"
    
    ## start _simulations_ early to test offsets
    , time = mp_sim_offset(30, 0, "daily")
    
    ## start optimization with beta away from the true value
    , default = list(beta = 0.9)
  )
  mp_optimize(cal_dates)
  fitted_data_dates = mp_trajectory(cal_dates)
  (sim_dates
    |> ggplot()
    + geom_point(aes(time, value))
    + geom_line(aes(time, value), data = fitted_data_dates)
    + theme_bw()
  )
  cc_dates = mp_tmb_coef(cal)
  expect_identical(cc, cc_dates)
})
