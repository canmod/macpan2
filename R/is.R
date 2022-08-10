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
