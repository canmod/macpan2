#' Fixed Effects
#'
#' @param model Object that contains information about fitted fixed effects
#' @returns A data frame that describes the fitted fixed effects.
#' @export
mp_fixed_effects = function(model) UseMethod("mp_fixed_effects")

#' @export
mp_fixed_effects.TMBSimulator = function(model) {
  fixed_effects = model$tmb_model$params$data_frame()
  sd_report = model$sdreport()
  fixed_effects$estimate = sd_report$par.fixed
  fixed_effects$std_error = sqrt(diag(sd_report$cov.fixed))
  fixed_effects
}



