#' @exportClass symb
setClass('symb', representation(v = "character", d = 'integer'),
  validity = function(object) {
    if (length(object@d) != 2L) return('only two dimensions are allowed')
    if (length(object@v) != prod(object@d)) return('inconsistent dimensions')
  }
)

#' @exportClass symb_scal
setClass('symb_scal', contains = 'symb',
  validity = function(object) {
    if (length(object@v) != 1L) return('not a scalar')
    if (!all(object@d == 1L)) return('not a scalar')
    TRUE
  }
)

#' @exportClass symb_vec
setClass('symb_vec', contains = 'symb',
  validity = function(object) {
    if (length(object@v) != object@d[1L]) return('not a vector')
    if (object@d[2L] != 1L) return('not a vector')
    TRUE
  }
)

#' @exportClass symb_mat
setClass('symb_mat', contains = 'symb')


#' @export
setGeneric('is_scal',
  function(x) {
    (dim_1(x) == 1L) & (dim_1(x) == dim_2(x))
  }
)

#' @export
setGeneric('is_vec',
  function(x) {
    (dim_2(x) == 1L)
  }
)

#' @export
setGeneric('is_mat',
  function(x) dim_len(x) == 2L
)

#' @exportMethod is_scal
setMethod('is_scal', c(x = 'symb_scal'), function(x) TRUE)

#' @exportMethod is_vec
setMethod('is_vec', c(x = 'symb_vec'), function(x) TRUE)

#' @exportMethod is_mat
setMethod('is_mat', c(x = 'symb_mat'), function(x) TRUE)

#' @export
symb = function(...) {
  l = list(...)
  id_scal = unlist(lapply(l, is_scal))
  id_vec = unlist(lapply(l, is_vec))
  id_mat = unlist(lapply(l, is_mat))
  if (all(id_scal) & (length(l) == 1L)) {
    return(symb_scal(unlist(l)))
  }
  if (all(id_vec)) {
    return(symb_vec(unlist(l)))
  }
  if (all(id_mat) * (length(l) == 1L)) {
    return(symb_mat(unlist(l)))
  }
  stop('invalid input')
}

#' @describeIn symb
#' @export
symb_scal = function(x) {
  new('symb_scal', v = x, d = c(1L, 1L))
}

#' @describeIn symb
#' @export
symb_vec = function(x) {
  new('symb_vec', v = x, d = c(length(x), 1L))
}

#' @describeIn symb
#' @export
symb_mat = function(x) {
  new('symb_mat', v = as.character(x), d = dim(x))
}

# setGeneric("paste_symb", standardGeneric('paste_symb'))
# setMethod("paste_symb", c(x = 'symb', y = 'symb', sep = 'character'),
#   function(x, y, sep) {
#     paste0(wrap_paren(x)@v, wrap_paren(y)@v)
#   }
# )


#' @exportMethod dim
setMethod("dim", c(x = "symb"), function(x) {x@d})

#' @export
equal_dims = function(x, y) {
  all(dim_def(x) == dim_def(y))
}

#' @export
compat_dims = function(x, y) {
  equal_dims(x, y) | is_scal(x) | is_scal(y)
}

#' @exportMethod as.matrix
setMethod('as.matrix', c(x = 'symb'),
  function(x) {
    matrix(x@v, x@d[1], x@d[2])
  }
)

#' @export
tildify = function(x) paste0('~', x)


## element-wise binary operators

as_char_or_num = function(x) {
  if (is.numeric(x)) return(x)
  as.matrix(x)
}
op_whitespace = function(x) {
  x = as.character(x)
  stopifnot(length(x) == 1L)
  paste(" ", trimws(x), " ", sep = "")
}

ewise_bin_op = function(op, e1, e2) {
  x = as_char_or_num(e1)
  y = as_char_or_num(e2)
  stopifnot(compat_dims(e1, e2))
  symb(paste(wrap_paren(x), wrap_paren(y), sep = op_whitespace(op)))
}

#' @exportMethod `*`
setMethod(
  "*",
  c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) ewise_bin_op("*", e1, e2)
)

setMethod(
  "*",
  c(e1 = 'symb', e2 = 'numeric'),
  function(e1, e2) ewise_bin_op("*", e1, e2)
)

setMethod("*", c(e1 = 'numeric', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1, e2@v, sep = ' * ')))
  }
)

#' @exportMethod `/`
setMethod("/", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' * ')))
  }
)

#' @exportMethod `+`
setMethod("+", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' + ')))
  }
)

#' @exportMethod `+`
setMethod("+", c(e1 = 'numeric', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1, e2@v, sep = ' + ')))
  }
)

#' @exportMethod `-`
setMethod("-", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2)) # no recycling!
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' - ')))
  }
)

#' @exportMethod `^`
setMethod("^", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(paste(wrap_paren(e1@v), wrap_paren(e2@v), sep = ' ^ '))
  }
)

#' @exportMethod sum
setMethod('sum', c(x = 'symb'),
  function(x, ..., na.rm = FALSE) {
    l = unlist(lapply(c(list(x), list(...)), slot, 'v'))
    symb_scal(paste(l, collapse = ' + '))
  }
)

# setMethod("%*%", c(x = 'symb_mat', y = 'symb_mat'),
#   function(x, y) {
#     stopifnot(ncol(x) == nrow(y))
#     t(as.matrix(x)) %*% as.matrix(y)
#   }
# )

#' @export
setGeneric('wrap_paren',
  function(x) {
    paste0('(', x, ')')
  }
)

#' @exportMethod wrap_paren
setMethod('wrap_paren', c(x = 'symb'),
  function(x) {
    x@v = wrap_paren(x@v)
    x
  }
)
wrap_paren_if_necessary = function(x) {
  parse_char(x)
}

setGeneric('has_plus', function(x) grepl('\\+', x))
setMethod('has_plus', c(x = 'symb'), function(x) has_plus(x@v))
