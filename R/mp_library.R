#' @export
mp_library = function(...) {
  system.file("model_library", ..., package = "macpan2") |> Compartmental2()
}

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

## TODO: change model_starter to mp_model_starter

#' Model Starter
#'
#' Create a directory with a template model definition.
#'
#' @param starter_name Currently can only be \code{sir}.
#' @param dir String giving the path to a directory for copying the
#' template model definition.
#'
#' @export
model_starter = function(starter_name, dir) {
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
