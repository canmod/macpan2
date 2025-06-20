mp_more_steps = function(model, steps_to_add) {
  model = assert_cls(model, "TMBCalibrator", match.call(), "?mp_tmb_calibrator")
  steps = model$simulator$tmb_model$time_steps$time_steps
  model$simulator$replace$time_steps(steps + steps_to_add)
}

#' Make a Forecaster
#' 
#' A forecaster is an object that can make forecasts. It is constructed from
#' \code{\link{mp_tmb_calibrator}}, which is an object that can be used to
#' calibrate the parameters of a model -- typically by fitting the model to
#' data. The main difference between a calibrator and a forecaster is that
#' the latter runs for more time steps, past the last time step of the 
#' calibrator. A forecaster object can be used to generate different types of
#' forecasts using the \code{\link{mp_trajectory}} family of functions.
#' 
#' @param calibrator Object made using \code{\link{mp_tmb_calibrator}}, which
#' can be calibrated to data.
#' @param forecast_period_time_steps The number of time steps to project
#' beyond the period with data.
#' @param outputs An optional character vector of variables in the model
#' to forecast. If this argument is omited, then the forecaster will inherit
#' the outputs of the `calibrator` object. Note that if you prefix the name
#' of an output using `log_`, `logit_`, or `sqrt_` then the transformed
#' version of the outputs will be simulated. This technique is useful for
#' confidence intervals produced by \code{\link{mp_trajectory_sd}} that
#' go out of the valid range of values (e.g., use `log_` for confidence 
#' intervals of negative state variables).
#' @param data An optional data frame containing the data that were fitted to.
#' Typically this argument will be unused, because by default the data are
#' stored in the `calibrator` unless you want to avoid making too many copies 
#' of the data.
#' @param tv An optional replacement for the `tv` parameter in the 
#' \code{\link{mp_tmb_calibrator}} function.
#' @param default An optional list of default model variables (e.g., parameters
#' initial values of state variables) to override calibrated values.
#' @param inits An optional list of initial values for the state variables.
#' These initial values can be added to the `default` list with identical 
#' results, but adding them to `inits` is better practice because it makes it 
#' clear that they are initial values that will change as the state updates.
#' 
#' @export
mp_forecaster = function(calibrator, forecast_period_time_steps
    , outputs = NULL, data = NULL, tv = NULL
    , default = list(), inits = list()
  ) {
  default = c(default, inits)
  spec = mp_optimized_spec(calibrator, "original")
  if (!is.null(outputs)) spec = mp_tmb_implicit_trans(spec, outputs)
  
  args = calibrator$cal_args
  args$default = default
  args$spec = spec
  if (!is.null(data)) args$data = data
  if (!is.null(outputs)) args$outputs = outputs
  if (!is.null(tv)) args$tv = tv
  if (is.null(args$time)) {
    args$time = calibrator$time_steps_obj$extended_time_arg(forecast_period_time_steps)
  } else {
    args$time = args$time$extend(forecast_period_time_steps)
  }
  do.call(mp_tmb_calibrator, args)
}
