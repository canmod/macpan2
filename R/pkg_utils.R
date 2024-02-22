## from broom
assert_dependency <- function(library_name) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    stop(sprintf("Please install the %s package.", library_name))
  }
}

assert_dependencies = function(...) {
  library_names = list(...) |> unlist() |> as.character()
  exist = vapply(library_names, requireNamespace, logical(1L), quietly = TRUE)
  if (!all(exist)) {
    stop(
      sprintf(
        "Please install the following packages:\n%s",
        paste0(library_names[!exist], collapse = ", ")
      )
    )
  }
}
