## from broom
assert_dependency <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(sprintf("Please install the %s package.", package_name))
  }
}

assert_dependencies = function(...) {
  package_names = list(...) |> unlist() |> as.character()
  exist = vapply(package_names, requireNamespace, logical(1L), quietly = TRUE)
  if (!all(exist)) {
    stop(
      sprintf(
        "Please install the following packages:\n%s",
        paste0(package_names[!exist], collapse = ", ")
      )
    )
  }
}

assert_function_dependency = function(function_object) {
  function_object |> is.function() |> try() |> isTRUE()
}
