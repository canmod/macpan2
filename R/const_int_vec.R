#' Constant Integer Vectors
#'
#' Make a list of integer vectors available for engine methods.
#'
#' @param ... Named arguments, each of which can be coerced to an integer vector.
#' @noRd
IntVecs = function(...) {
  self = Base()

  # Args
  self$list = lapply(list(...), as.integer)
  
  # Arg validity
  if (length(self$list) == 0L) self$list = list(integer())
  # missing_ints = lapply(self$list, is.na) |> vapply(any, logical(1L))
  # if (any(missing_ints)) {
  #   stop("Integer vectors cannot have any missing values.")
  # }
  
  # Standard Methods
  self$data_arg = function() {
    list(
      const_int_vec = unlist(self$list, use.names = FALSE),
      const_n_int_vecs = unlist(lapply(self$list, length), use.names = FALSE)
    )
  }
  self$const_names = function() names(self$list)

  return_object(self, "IntVecs")
}

#' @export
names.IntVecs = function(x) names(x$list)
