## from broom
assert_dependency <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(sprintf("Please install the %s package.", package_name))
  }
}

plural_dependency_asserter = function(msg, use_require = FALSE) function(...) {
  pkg_nms = list(...) |> unlist() |> as.character()
  exist = vapply(pkg_nms, requireNamespace, logical(1L), quietly = TRUE)
  if (!all(exist)) {
    stop(
      sprintf("%s:\n%s"
        , msg
        , paste0(pkg_nms[!exist], collapse = ", ")
      )
    )
  }
  if (use_require) {
    for (pkg in pkg_nms) {
      message("Loading package ", pkg, ", which is required my mp_plot_layout.")
      if (!require(pkg, character.only = TRUE)) {
        sprintf("\nPlease run the following commands:%s"
          , sprintf("\nlibrary(%s)", pkg_nms)
        ) |> stop()
      }
    }
  }
}


assert_dependencies = plural_dependency_asserter("Please install the following packages", use_require = FALSE)
assert_dependencies2 = plural_dependency_asserter("Please install the following packages from Bioconductor", use_require = TRUE)
assert_dependencies3 = plural_dependency_asserter("Please install the following packages from Bioconductor", use_require = FALSE)


assert_function_dependency = function(function_object) {
  function_object |> is.function() |> try() |> isTRUE()
}
