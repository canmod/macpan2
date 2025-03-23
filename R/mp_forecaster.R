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
#' 
#' @export
mp_forecaster = function(calibrator, forecast_period_time_steps
    , outputs = NULL, data = NULL, tv = NULL, default = list()
  ) {
  spec = mp_optimized_spec(calibrator, "original")
  
  if (!is.null(outputs)) {
    all_possible = spec$all_formula_vars()
    
    simple_outputs = intersect(outputs, all_possible)
    complex_outputs = setdiff(outputs, all_possible)
    trans_outputs = grep("^(log|logit|sqrt)_", complex_outputs, value = TRUE)
    
    log_outputs = sub("^log_", "", complex_outputs) |> intersect(all_possible)
    logit_outputs = sub("^logit_", "", complex_outputs) |> intersect(all_possible)
    sqrt_outputs = sub("^sqrt_", "", complex_outputs) |> intersect(all_possible)
    
    good_outputs = c(simple_outputs, trans_outputs)
    bad_outputs = setdiff(outputs, good_outputs)
    if (length(bad_outputs) > 0L) {
      mp_wrap(
        "The following outputs were required but not available in the model"
      , bad_outputs
      )
    }
    if (length(log_outputs) > 0L) {
      spec = (spec
        |> mp_tmb_insert_trans(log_outputs, mp_log)
        |> mp_tmb_insert_trans(logit_outputs, mp_logit)
        |> mp_tmb_insert_trans(sqrt_outputs, mp_sqrt)
      )
    }
  }
  
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
