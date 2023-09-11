#' Constant Integer Vectors
#'
#' Make a list of integer vectors available for engine methods.
#'
#' @param ... Named arguments, each of which can be coerced to an integer vector.
#' @export
ConstIntVecs = function(...) {
  self = Base()

  # Args
  self$list = lapply(list(...), as.integer)
  if (length(self$list) == 0L) self$list = list(integer())

  # Standard Methods
  self$data_arg = function() {
    # DATA_IVECTOR(const_int_vec);
    # DATA_IVECTOR(const_n_int_vecs);
    list(
      const_int_vec = unlist(self$list, use.names = FALSE),
      const_n_int_vecs = unlist(lapply(self$list, length), use.names = FALSE)
    )
  }
  self$const_names = function() names(self$list)

  return_object(self, "ConstIntVecs")
}
