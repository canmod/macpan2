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


List = function(...) {
  self = Base()
  self$.list = nlist(...)
  self$add = function(...) {
    self$.list = c
  }
  return_object(self, "List")
}
