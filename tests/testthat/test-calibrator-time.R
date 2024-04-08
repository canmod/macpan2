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
    |>  dplyr::mutate(matrix="I") # not actually I, just using as an example
  )
  
  sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
  
  # get a lapply warning 
  sir_calib  = mp_tmb_calibrator(sir,
                                 , data=sir_obs
                                 , traj = "I"
                                 , par = "beta")
  # dates are there
  expect_identical(sir_calib$cal_spec$integers$obs_times_I, 1:318)
  
  # this seg faults
  expect_identical(mp_trajectory(sir_calib)$time, 1:318)
})
