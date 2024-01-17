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
