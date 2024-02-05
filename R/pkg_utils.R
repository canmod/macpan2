## from broom
assert_dependency <- function(library_name) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    stop(sprintf("Please install the %s package.", library_name))
  }
}
