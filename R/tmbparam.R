#' Default Parameter Vector
#'
#' Get and set current default value for the parameter vector of
#' a \code{\link{flexmodel}}
#'
#' @param model \code{\link{flexmodel}} object
#' @export
params_default = function(model) {
  UseMethod("params_default")
}

#' @param value object the same type and format as \code{model$params}
#' \code{new_params} will be merged with \code{model$params}
#' @rdname params_default
#' @export
`params_default<-` = function(model, value) {
  UseMethod("params_default<-")
}

#' @export
params_default.flexmodel = function(model) {
  model$params
}

#' @export
`params_default<-.flexmodel` = function(model, value) {
  model$params[names(value)] = value
  model
}


params_init = function(model) {
  UseMethod("params_init")
}

params_arg = function(model) {
  UseMethod("params_init")
}


