mp_more_steps = function(model, steps_to_add) {
  model = assert_cls(model, "TMBCalibrator", match.call(), "?mp_tmb_calibrator")
  steps = model$simulator$tmb_model$time_steps$time_steps
  model$simulator$replace$time_steps(steps + steps_to_add)
}
