#' @exportClass form
setClass('form', representation(l = "list", d = 'integer'),
  validity = function(object) {
    if (length(object@d) != 2L) return('only two dimensions are allowed')
    if (length(object@l) != prod(object@d)) return('inconsistent dimensions')
    if (!all(unlist(lapply(object@l, is_one_sided_formula)))) {
      return('every element of l slot must be a one sided formula')
    }
    if (!all(unlist(lapply(lapply(object@l, environment), is_form_env)))) {
      return('invalid formula environment')
    }
  }
)

#' @exportClass form_scal
setClass('form_scal', contains = 'form',
  validity = function(object) {
    if (length(object@l) != 1L) return('not a scalar')
    if (!all(object@d == 1L)) return('not a scalar')
    TRUE
  }
)

#' @exportClass form_vec
setClass('form_vec', contains = 'form',
  validity = function(object) {
    if (length(object@l) != object@d[1L]) return('not a vector')
    if (object@d[2L] != 1L) return('not a vector')
    TRUE
  }
)

#' @exportClass form_mat
setClass('form_mat', contains = 'form')


is_one_sided_formula = function(x) {
  is.call(x) & (as.character(x[[1]]) == "~") & (length(x) == 2L)
}

#' @export
form = function(symb_obj, env) {
  l = lapply(lapply(symb_obj@v, tildify), as.formula)
  for (i in seq_along(l)) {
    environment(l[[i]]) = env
  }
  cls = sub('symb', 'form', class(symb_obj))
  new(cls, l = l, d = symb_obj@d)
}
