#' Description of Model Parameterization
#' 
#' @param model Parameterized model, probably produced using 
#' \code{\link{mp_tmb_calibrator}}.
#' @param types Vector indicating what kinds of parameters should be
#' included, `"fixed"` for fixed-effect-type parameters and `"random"`
#' for random-effect-type.
#' 
#' @export
mp_parameterization = function(model, types = c("fixed", "random")) {
  UseMethod("mp_parameterization")
}

#' @export
mp_parameterization.TMBSimulator = function(model, types = c("fixed", "random")) {
  types = match.arg(types, several.ok = TRUE)
  frame_list = list(empty = empty_frame(c("par_id", "mat", "row", "col", "default", "current", "type")))
  if ("fixed" %in% types) frame_list$fixed = model$current$params_frame()
  if ("random" %in% types) frame_list$random = model$current$random_frame()
  bind_rows(frame_list, .id = "type")
}

#' @export
mp_parameterization.TMBCalibrator = function(model, types = c("fixed", "random")) {
  mp_parameterization(model$simulator, types)
}
