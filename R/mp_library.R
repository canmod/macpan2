
mp_library = function(...) {
  stop("under construction")
}

#' Read Item from a Model Library
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
#' @param alternative_specs If \code{TRUE}, return a list of alternative
#' specification objects. For models without alternatives this will cause
#' the return value to be a list with one element containing a spec object.
#' 
#' @seealso [mp_show_models()]
#' 
#' @examples
#' mp_tmb_library(
#'     "starter_models"
#'   , "si"
#'   , package = "macpan2"
#' )
#' 
#' @concept create-model-spec
#' @export
mp_tmb_library = function(..., package = NULL, alternative_specs = FALSE) {
  if (is.null(package)) {
    model_directory = file.path(...)
  } else {
    model_directory = system.file(..., package = package)
  }
  if (!dir.exists(model_directory)) {
    stop("Library model directory does not exist.")
  }
  def_env = new.env(parent = parent.frame())
  sys.source(file.path(model_directory, "tmb.R")
    , envir = def_env
    , chdir = TRUE
  )
  if (alternative_specs) {
    if (inherits(def_env$specs, "list")) {
      if (all(vapply(def_env$specs, inherits, logical(1L), "TMBModelSpec"))) {
        return(def_env$specs)
      }
    }
  } else {
    if (inherits(def_env$spec, "TMBModelSpec")) {
      return(def_env$spec)
    }
    if (inherits(def_env$specs[[1L]], "TMBModelSpec")) {
      def_env$spec = def_env$specs[[1L]]
      return(def_env$spec)
    }
  }
  stop("Malformed model library entry.")
}

#' @describeIn mp_tmb_library List of one model specification for each model
#' in the library.
#' @export
mp_tmb_entire_library = function() {
  sapply(
      mp_show_models()$Directory
    , \(model_dir) {
        mp_tmb_library("starter_models", model_dir, package = "macpan2")
    }
    , simplify = FALSE
    , USE.NAMES = TRUE
  )
}

#' Copy Existing Model as a Starting Point
#'
#' Create a directory with a template model definition.
#'
#' @param starter_name Currently can only be \code{sir}.
#' @param dir String giving the path to a directory for copying the
#' template model definition.
#'
#' @concept create-model-spec
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
