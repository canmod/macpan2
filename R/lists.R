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

#' Extract Expressions by Name
#'
#' @param x Object containing named expressions.
#' @param ... Character vectors containing names of expressions.
#'
#' @returns An object the same type as \code{x} but only with
#' those expressions identified by the names in \code{...}.
#'
#' @export
mp_extract_exprs = function(x, ...) UseMethod("mp_extract_exprs")

#' @export
mp_extract_exprs.list = function(x, ...) {
  nms = (list(...)
    |> lapply(as.character)
    |> unlist(use.names = FALSE)
    |> unique()
  )
  x[names(x) %in% nms]
}

#' @export
mp_extract_exprs.ExprList = function(x, ...) {
  nms = (list(...)
    |> lapply(as.character)
    |> unlist(use.names = FALSE)
    |> unique()
  )
  ExprList(
    before = mp_extract_exprs(x$before, ...),
    during = mp_extract_exprs(x$during, ...),
    after = mp_extract_exprs(x$after, ...),
    .simulate_exprs = intersect(x$.simulate_exprs, nms)
  )
}

#' @export
mp_extract_exprs.DynamicModel = function(x, ...) {
  mp_extract_exprs(x$expr_list, ...)
}


#' @export
mp_combine_exprs = function(...) {
  UseMethod("mp_combine_exprs")
}

#' @export
mp_combine_exprs.list = function(...) c(...)
