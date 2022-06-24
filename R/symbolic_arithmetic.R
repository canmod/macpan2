setClass('symb', representation(v = "character", d = 'integer'),
  validity = function(object) {
    if (length(object@d) != 2L) return('only two dimensions are allowed')
    if (length(object@v) != prod(object@d)) return('inconsistent v and d slot')
  }
)
setClass('symb_scal', contains = 'symb',
  validity = function(object) {
    if (length(object@v) != 1L) return('not a scalar')
    if (!all(object@d == 1L)) return('not a scalar')
    TRUE
  }
)
setClass('symb_vec', contains = 'symb',
  validity = function(object) {
    if (length(object@v) != object@d[1L]) return('not a vector')
    if (object@d[2L] != 1L) return('not a vector')
    TRUE
  }
)
setClass('symb_mat', contains = 'symb')

dim_gen = function(x) {
  if (is.null(dim(x))) return(c(length(x), 1L))
  d = dim(x)
  if (length(d) != 2L) stop('more than two dimensions are not allowed')
  d
}
dim_len = function(x) {
  length(dim_gen(x))
}
dim_1 = function(x) {
  dim_gen(x)[1]
}
dim_2 = function(x) {
  dim_gen(x)[2]
}

setGeneric('is_scal',
  function(x) dim_1(x) == dim_2(x)
)
setGeneric('is_vec',
  function(x) dim_2(x) == 1L
)
setGeneric('is_mat',
  function(x) dim_len(x) == 2L
)
setMethod('is_scal', c(x = 'symb_scal'), function(x) TRUE)
setMethod('is_vec', c(x = 'symb_vec'), function(x) TRUE)
setMethod('is_mat', c(x = 'symb_mat'), function(x) TRUE)

symb = function(x) {
  if (is_scal(x)) return(symb_scal(x))
  if (is_vec(x)) return(symb_vec(x))
  if (is_mat(x)) return(symb_mat(x))
  stop('invalid input')
}

symb_scal = function(x) {
  new('symb_scal', v = x, d = c(1L, 1L))
}

symb_vec = function(x) {
  new('symb_vec', v = x, d = c(length(x), 1L))
}

symb_mat = function(x) {
  new('symb_mat', v = as.character(x), d = dim(x))
}

# setGeneric("paste_symb", standardGeneric('paste_symb'))
# setMethod("paste_symb", c(x = 'symb', y = 'symb', sep = 'character'),
#   function(x, y, sep) {
#     paste0(wrap_paren(x)@v, wrap_paren(y)@v)
#   }
# )


setMethod("dim", c(x = "symb"), function(x) {x@d})

equal_dims = function(x, y) {
  all(dim_gen(x) == dim_gen(y))
}

compat_dims = function(x, y) {
  equal_dims(x, y) | is_scal(x) | is_scal(y)
}

setMethod('as.matrix', c(x = 'symb'),
  function(x) {
    matrix(x@v, x@d[1], x@d[2])
  }
)

setMethod('as.formula', c(object = 'symb_scal'),
  function(object) {
    as.formula(tildify(object@v))
  }
)

tildify = function(x) paste0('~', x)

setMethod("*", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(paste(wrap_paren(e1@v), wrap_paren(e2@v), sep = ' * '))
  }
)

setMethod("*", c(e1 = 'symb', e2 = 'numeric'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1@v, e2, sep = ' * ')))
  }
)

setMethod("*", c(e1 = 'numeric', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1, e2@v, sep = ' * ')))
  }
)


setMethod("/", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' * ')))
  }
)

setMethod("+", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2))
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' + ')))
  }
)

setMethod("-", c(e1 = 'symb', e2 = 'symb'),
  function(e1, e2) {
    stopifnot(compat_dims(e1, e2)) # no recycling!
    symb(wrap_paren(paste(e1@v, e2@v, sep = ' - ')))
  }
)

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

setGeneric('wrap_paren',
  function(x) {
    paste0('(', x, ')')
  }
)
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
