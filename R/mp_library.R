
mp_library = function(...) {
  stop("under construction")
}

#' TMB Library
#' 
#' Get a TMB model specification from a model library.
#' 
#' @param ... File path components pointing to a directory that
#' contains an R script that creates an object called `spec`, which
#' is produced by \code{\link{mp_tmb_model_spec}}.
#' @param package If \code{NULL}, \code{\link{file.path}} is used
#' to put together the \code{...} components but if \code{package}
#' is the name of a package (as a character string) then
#' \code{\link{system.file}} is used to put together the \code{...}
#' components.
#' 
#' @seealso [show_models()]
#' 
#' @examples
#' mp_tmb_library(
#'     "starter_models"
#'   , "si"
#'   , package = "macpan2"
#' )
#' 
#' @export
mp_tmb_library = function(..., package = NULL) {
  if (is.null(package)) {
    model_directory = file.path(...)
  } else {
    model_directory = system.file(..., package = package)
  }
  def_env = new.env(parent = parent.frame())
  sys.source(file.path(model_directory, "tmb.R")
    , envir = def_env
    , chdir = TRUE
  )
  def_env$spec
}

#' Model Starter
#'
#' Create a directory with a template model definition.
#'
#' @param starter_name Currently can only be \code{sir}.
#' @param dir String giving the path to a directory for copying the
#' template model definition.
#'
#' @export
mp_model_starter = function(starter_name, dir) {
  starter_dir = system.file("starter_models"
    , starter_name
    , package = "macpan2"
  )
  starter_files = list.files(starter_dir)
  required_files = c(
    tmb_engine_file = "tmb.R"
  )
  if (!all(required_files %in% starter_files)) {
    stop("Could not find a valid starter model by that name.")
  }

  starter_paths = setNames(
    file.path(starter_dir, required_files),
    names(required_files)
  )

  if (dir.exists(dir)) stop("Directory for the model already exists.")
  dir.create(dir, recursive = TRUE)

  file.copy(starter_paths, dir)
  
  ## TODO: handle the multi-engine case
  ## TODO: implement proper file update monitoring (e.g. Files objects)
  mp_tmb_library(dir)
}
