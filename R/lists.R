#' Self Naming List
#'
#' @param ... Objects to put into the list
#'
#' @export
nlist = function(...) {
    #browser()
    L = list(...)
    if (valid$char_no_empty$is_true(names(L))) return(L)
    snm = vapply(substitute(list(...)), deparse, character(1))[-1]
    if (is.null(nm <- names(L))) {
        nm = snm
    }
    if (any(nonames <- nm == "")) {
        nm[nonames] <- snm[nonames]
    }
    setNames(L, nm)
}

melt_matrix_int = function(x) {
  dm = dim(x)
  if (is.null(dm)) {
    row = seq_along(x) - 1L
    col = 0
  } else {
    row = rep(seq_len(dm[1]), each = dm[2]) - 1
    col = rep(seq_len(dm[2]), times = dm[1]) - 1
  }
  data.frame(row = row, col = col, value = as.vector(x))
}

melt_matrix = function(x, zeros_are_blank = TRUE) {
  dn = dimnames(x)
  nms = names(x)
  dm = dim(x)
  if (is.null(dm)) {
    if (zeros_are_blank) {
      col = ""
    } else {
      col = "0"
    }
    if (is.null(nms)) {
      if ((length(x) == 1L) & zeros_are_blank) {
        row = ""
      } else {
        row = as.character(seq_along(x) - 1)
      }
    } else {
      row = names(x)
    }
  } else if (is.null(dn)) {
    if ((dm[1] == 1L) & zeros_are_blank) {
      row = ""
    } else {
      row = as.character(rep(seq_len(dm[1]), each = dm[2]) - 1)
    }
    if ((dm[2] == 1L) & zeros_are_blank) {
      col = ""
    } else {
      col = as.character(rep(seq_len(dm[2]), times = dm[1]) - 1)
    }
  } else { 
    ## FIXME: need to check NULL rownames and NULL colnames separately
    row = rep(rownames(x), times = dm[2])
    col = rep(colnames(x), each = dm[1])
  }
  data.frame(row = row, col = col, value = as.vector(x))
}

melt_default_matrix_list = function(x, zeros_are_blank = TRUE) {
  if (length(x) == 0L) return(NULL)
  f = (x
   |> lapply(melt_matrix, zeros_are_blank)
   |> bind_rows(.id = "matrix")
  )
  rownames(f) = NULL
  f
}

clean_dimnames = function(dn) {
  if (!is.null(dn)) {
    if (identical(as.character(dn[[2L]]), "0")) dn[[2L]] = NULL
    if (identical(as.character(dn[[1L]]), "0")) dn[[1L]] = NULL
  }
  return(dn)
}
cast_default_matrix_list = function(x) {
  val_list = tapply(x$value, x$matrix, c, simplify = FALSE)
  row_list = tapply(x$row, x$matrix, c, simplify = FALSE) |> lapply(unique)
  col_list = tapply(x$col, x$matrix, c, simplify = FALSE) |> lapply(unique)
  dimnames = mapply(list, row_list, col_list, SIMPLIFY = FALSE) |> lapply(clean_dimnames)
  nrow = vapply(row_list, length, integer(1L))
  ncol = vapply(col_list, length, integer(1L))
  mapply(matrix, val_list, nrow, ncol, dimnames = dimnames, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}
cast_default_matrix_list = memoise(cast_default_matrix_list)

empty_named_list = function() list() |> setNames(character(0L))

assert_named_list = function(l) {
  if (is.null(names(l))) {
    if (length(l) == 0L) {
      l = setNames(list(), character())
    } else {
      stop("Developer error: missing names")
    }
  }
  l
}

self_named_vector = function(...) c(...) |> setNames(c(...))


# Extract Expressions by Name -- Experimental
#
# @param x Object containing named expressions.
# @param ... Character vectors containing names of expressions.
#
# @returns An object the same type as \code{x} but only with
# those expressions identified by the names in \code{...}.
#

# mp_extract_exprs = function(x, ...) UseMethod("mp_extract_exprs")
# 
# mp_extract_exprs.list = function(x, ...) {
#   nms = (list(...)
#     |> lapply(as.character)
#     |> unlist(use.names = FALSE)
#     |> unique()
#   )
#   x[names(x) %in% nms]
# }
# 
# mp_extract_exprs.ExprList = function(x, ...) {
#   nms = (list(...)
#     |> lapply(as.character)
#     |> unlist(use.names = FALSE)
#     |> unique()
#   )
#   ExprList(
#     before = mp_extract_exprs(x$before, ...),
#     during = mp_extract_exprs(x$during, ...),
#     after = mp_extract_exprs(x$after, ...),
#     .simulate_exprs = intersect(x$.simulate_exprs, nms)
#   )
# }
# 
# mp_extract_exprs.DynamicModel = function(x, ...) {
#   mp_extract_exprs(x$expr_list, ...)
# }
# 

# mp_combine_exprs = function(...) {
#   UseMethod("mp_combine_exprs")
# }
# 
# mp_combine_exprs.list = function(...) c(...)
