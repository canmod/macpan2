#' Test TMB Model Spec (Developer Tool)
#' 
#' Create a TMB object that runs for one time-step and does not do 
#' automatic differentiation. This is useful for debugging without having
#' many messages repeated each pass through the template.
#' @noRd
#' @export
mp_tmb_test = function(model, time_steps = 2) UseMethod("mp_tmb_test")

#' @export
mp_tmb_test.TMBModelSpec = function(model, time_steps = 1) {
  tmb_model = model$tmb_model(time_steps = time_steps, initialize_ad_fun = FALSE)
  ad = do.call(
    TMB::MakeADFun,
    c(tmb_model$make_ad_fun_arg(), list(checkParameterOrder = FALSE, type = "Fun"))
  )
  NULL
}
