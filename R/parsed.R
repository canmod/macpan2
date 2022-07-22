#' @exportClass decomp
setClass('decomp', representation(l = "list", d = 'integer'),
  validity = function(object) {
    if (length(object@d) != 2L) return('only two dimensions are allowed')
    if (length(object@l) != prod(object@d)) return('inconsistent dimensions')
  }
)

#' @exportClass decomp_scal
setClass('decomp_scal', contains = 'decomp',
  validity = function(object) {
    if (length(object@l) != 1L) return('not a scalar')
    if (!all(object@d == 1L)) return('not a scalar')
    TRUE
  }
)

#' @exportClass decomp_vec
setClass('decomp_vec', contains = 'decomp',
  validity = function(object) {
    if (length(object@l) != object@d[1L]) return('not a vector')
    if (object@d[2L] != 1L) return('not a vector')
    TRUE
  }
)

#' @exportClass decomp_mat
setClass('decomp_mat', contains = 'decomp')

#' @export
decomp = function(form_obj) {
  cls = sub('form', 'decomp', class(form_obj))
  parse_expr = make_expr_parser(finalizer = finalizer_index)
  l = lapply(form_obj@l, parse_expr)
  new(cls, l = l, d = form_obj@d)
}
