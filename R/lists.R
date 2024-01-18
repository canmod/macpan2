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


melt_matrix = function(x) {
  dn = dimnames(x)
  nms = names(x)
  dm = dim(x)
  if (is.null(dm)) {
    col = ""
    if (is.null(nms)) {
      if (length(x) == 1L) {
        row = ""
      } else {
        row = seq_along(x) |> as.character()
      }
    } else {
      row = names(x)
    }
  } else if (is.null(dn)) {
    if (dm[1] == 1L) {
      row = ""
    } else {
      row = rep(seq_len(dm[1]), each = dm[2]) |> as.character()
    }
    if (dm[2] == 1L) {
      col = ""
    } else {
      col = rep(seq_len(dm[2]), times = dm[1]) |> as.character()
    }
  } else {
    row = rep(rownames(x), times = dm[2])
    col = rep(colnames(x), each = dm[1])
  }
  data.frame(row = row, col = col, value = as.vector(x))
}

melt_default_matrix_list = function(x) {
  f = (x
   |> lapply(melt_matrix)
   |> bind_rows(.id = "matrix")
  )
  rownames(f) = NULL
  f
}

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
