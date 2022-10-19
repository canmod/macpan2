# IsPipeline = function(...) {
#   test_functions = list(...)
#   all_functions = all(vapply(test_functions, is.function, logical(1L)))
#   if (!all_functions) stop("only functions are allowed in an IsPipeline")
#   function(x, verbose = FALSE) {
#     for (i in seq_along(test_functions)) {
#       if (verbose) {
#         print("================")
#         print("IsPipeline Stage:")
#         print(body(test_functions[[i]]))
#         print(as.list(environment(test_functions[[i]])))
#       }
#       if (!test_functions[[i]](x)) {
#         warning("\na test function has failed.\nrun again with verbose = TRUE")
#         return(FALSE)
#       }
#     }
#     return(TRUE)
#   }
# }
#
# IsStage = function(test_function) {
#   function(x) {
#     test_function(x)
#   }
# }
#
# IsDimRange = function(upper, lower) {
#   function(x) {
#     l = length(dim(x))
#     (lower <= l) & (upper >= l)
#   }
# }
#
# AllInList = function(test_function) {
#   function(x) {
#     all(vapply(x, test_function, logical(1L)))
#   }
# }
#
# HomoList = function(homogenizing_function) {
#   function(x) {
#     length(unique(lapply(x, homogenizing_function))) == 1L
#   }
# }
#
# Complement = function(test_function) {
#   function(x) {
#     !test_function(x)
#   }
# }
#
# NamesEqual = function(...) {
#   test_names = unlist(recursive = TRUE, as.character(list(...)))
#   function(x) {
#     isTRUE(all.equal(names(x), test_names))
#   }
# }

#' @export
is_plain_list = function(x) {
  inherits(x, "list")
}

#' @export
is_list = is.list

#' @export
is_length_zero = function(x) {
  length(x) == 0L
}

#' @export
is_length_positive = function(x) {
  length(x) > 0L
}

#' @export
is_el_blank = function(x) {
  nchar(x) == 0L
}

#' @export
is_name_or_number = function(x) {
  is.name(x) | is.numeric(x)
}

#' @export
is_scalar = function(x) {
  is.numeric(x) & (length(x) == 1L)
}

#' @export
is_vector = function(x) {
  is.numeric(x) & is.null(dim(x))
}

#' @export
is_matrix = function(x) {
  is.numeric(x) & !is.null(dim(x))
}

#' @export
is_numeric_list = function(x) {
  all(vapply(x, is.numeric, logical(1L)))
}

#' @export
is_logical_list = function(x) {
  all(vapply(x, is.logical, logical(1L)))
}

# @export
#is_under_3_dims = IsUnderNDims(3L)

# @export
#is_under_3_dims_list = IsHomoList(is_under_3_dims)

#' @export
is_nlist_of_funcs = function(x) {
  funcs_in_global = lapply(names(ee$valid_funcs), get, envir = .GlobalEnv)
  funcs_in_list = unname(ee$valid_funcs)
  is.list(x) & isTRUE(all.equal(funcs_in_global, funcs_in_list))
}

#' @export
is_matrix_list = function(x) {
  if (is.null(x)) return(FALSE)
  if (!is.list(x)) return(FALSE)
  all(vapply(x, is_matrix, logical(1L)))
}

#' @export
is_form_env = function(env) {
  vars_exist = !is.null(env$valid_vars)
  stopifnot(vars_exist)

  funcs_exist = !is.null(env$valid_funcs)
  stopifnot(funcs_exist)

  good_vars = is_matrix_list(env$valid_vars)
  stopifnot(good_vars)

  good_funcs = is_nlist_of_funcs(env$valid_funcs)
  stopifnot(good_funcs)
}

#' @export
has_valid_names = function(x) {
  !is.null(names(x)) & isTRUE(!any(duplicated(names(x))))
}

#' @export
keep = function(x, is_fun) {
  x[is_fun(x)]
}

#' @export
keep_not = function(x, is_fun) {
  x[!is_fun(x)]
}
