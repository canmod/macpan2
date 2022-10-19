#' Self Naming List
#' @export
nlist = function(...) {
    L = list(...)
    snm = vapply(substitute(list(...)), deparse, character(1))[-1]
    if (is.null(nm <- names(L))) {
        nm = snm
    }
    if (any(nonames <- nm == "")) {
        nm[nonames] <- snm[nonames]
    }
    setNames(L, nm)
}

#' Index a Named List of Character Vectors
#' @export
ilist = function(x) {
  x = lapply(x, names)
  nms = unlist(unname(x))
  var_type_index = setNames(
    unlist(mapply(rep, seq_along(x), each = unlist(lapply(unname(x), length)))),
    nms
  )
  var_index = setNames(
    unlist(lapply(unname(x), seq_along)),
    nms
  )
  nlist(var_type_index, var_index)
}

