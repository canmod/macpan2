##'
##' \pkg{macpan2}
##'
##' This R package is a do-over of the
##' [McMasterPandemic](https://github.com/mac-theobio/mcmasterpandemic)
##' package on compartmental epidemic modelling for
##' forecasting and analysis of infectious diseases.
##' 
##' @aliases macpan2 macpan2-pkg
##' @keywords internal
"_PACKAGE"

#' Version of `macpan2`
#' 
#' @param model `macpan2` model object.
#' @return Object of type \code{\link{package_version}} giving the version of
#' `macpan2` that produced `model`.
#' @export
mp_version = function(model) UseMethod("mp_version")

get_ver = function(model) {
  if (!"macpan2_version" %in% names(model)) return(NA_character_)
  return(model$macpan2_version)
}

#' @export
mp_version.TMBModelSpec = function(model) get_ver(model)

#' @export
mp_version.TMBModelSimulator = function(model) get_ver(model)

#' @export
mp_version.TMBModel = function(model) get_ver(model)

#' @export
mp_version.TMBCalibrator = function(model) mp_version(model$simulator)
