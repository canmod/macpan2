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
