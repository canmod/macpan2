#' Self Naming List
#' @export
nlist <- function(...) {
    L <- list(...)
    snm <- vapply(substitute(list(...)), deparse, character(1))[-1]
    if (is.null(nm <- names(L))) {
        nm <- snm
    }
    if (any(nonames <- nm == "")) {
        nm[nonames] <- snm[nonames]
    }
    setNames(L, nm)
}

#' Index a Named List of Character Vectors
#' @export
ilist = function(x) {
  setNames(
    mapply(
      list,
      var_type_index = unlist(mapply(rep, seq_along(x), each = unlist(lapply(unname(x), length)))),
      var_index = unlist(lapply(unname(x), seq_along)),
      SIMPLIFY = FALSE
    ),
    unlist(unname(x))
  )
}

