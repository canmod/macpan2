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

#' @export
mp_version.TMBModelSpec = function(model) get_mod_ver(model)

#' @export
mp_version.TMBSimulator = function(model) get_mod_ver(model)

#' @export
mp_version.TMBModel = function(model) get_mod_ver(model)

#' @export
mp_version.TMBCalibrator = function(model) mp_version(model$simulator)

get_mod_ver = function(model) {
  if (!"macpan2_version" %in% names(model)) return(NA_character_)
  return(model$macpan2_version)
}

#' @importFrom utils packageVersion
get_pkg_ver = function(pkg = "macpan2") {
  ver = try(packageVersion(pkg), silent = TRUE)
  if (inherits(ver, "try-error")) {
    warning(
      sprintf("The %s package ", pkg),
      "is not currently installed, so its version can't be checked. ",
      sprintf("Even if %s is already loaded ", pkg),
      "and seems to work, it might not be compatible with the ",
      "models being used."
    )
  }
  return(ver)
}
