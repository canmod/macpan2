#' @export
setClass('dim_mock', representation(d = 'integer'))

#' @export
dim.dim_mock = function(x) x@d

#' @export
dim_mock = function(d) new('dim_mock', d = d)

#' @export
as_dim_mock = function(x) {
  if (is_scalar(x)) return(dim_mock(1L))
  if (is_vector(x)) return(dim_mock(length(x)))
  if (is_matrix(x)) return(dim_mock(dim(x)))
  stop("only allowed to mock numeric scalars, vectors, and matrices")
}

all_equal_dims = function(...) {
  length(unique(lapply(list(...), dim))) == 1L
}

elementwise = function(e1, e2) {
  if (isTRUE(all.equal(dim(e1), 1L))) return(e2)
  if (isTRUE(all.equal(dim(e2), 1L))) return(e1)
  stopifnot(all_equal_dims(e1, e2))
  e1
}

setMethod("+", c(e1 = 'dim_mock', e2 = 'dim_mock'), elementwise)
setMethod("-", c(e1 = 'dim_mock', e2 = 'dim_mock'), elementwise)
setMethod("*", c(e1 = 'dim_mock', e2 = 'dim_mock'), elementwise)
setMethod("/", c(e1 = 'dim_mock', e2 = 'dim_mock'), elementwise)
setMethod("^", c(e1 = 'dim_mock', e2 = 'dim_mock'), elementwise)
