#' Label Multiplication
#'
#' Create an object for multiplying label vectors together
#'
#' @param times String used to separate two strings being pasted together.
#' @param add String used to separate the items in a character vector being
#' collapsed into a character string.
#'
#' @return Object of class \code{LabelMultiply}.
#'
#' ## Methods
#'
#' * `$outer(x, y)`: Outer product by pasting together every pairwise
#' combination of the elements in `x` with those in `y`.
#' * `$direct(x, y)`: Direct product by pasting together the `i`th element
#' of `x` with that of `y`.
#' * `$inner(x, y)`: Inner product by collapsing the direct product of `x`
#' and `y` into a single string.
#'
#' ## Method Arguments
#'
#' * `x`: Character vector
#' * `y`: Character vector
#'
#' @examples
#' multiplier = LabelMultiply("_", ", ")
#' sir = c("S", "I", "R")
#' vax = c("unvax", "vax")
#' multiplier$outer(sir, vax)
#' multiplier$direct(sir, sir)
#' multiplier$inner(sir, sir)
#' @export
LabelMultiply = function(times = "_", add = ", ") {
  self = Base()
  self$.times = times # valid$char1$assert(times)
  self$.add = add # valid$char1$assert(add)

  self$outer = function(x, y) {
    x = valid$char$assert(x)
    y = valid$char$assert(y)
    paste(
      rep(x, times = length(y)),
      rep(y, each = length(x)),
      sep = self$.times
    )
  }
  self$direct = function(x, y) {
    #valid$char_eq_len$check(list(x, y))
    paste(x, y, sep = self$.times)
  }
  self$inner = function(x, y) {
    ## not sure what the use cases are for this, so
    ## the implementation might change. but I think it needs
    ## to be here for interface purposes.
    paste0(self$direct(x, y), collapse = self$.add)
  }
  self$prod = function(...) {
    do.call(paste0, c(list(...), list(collapse = self$.times)))
  }
  self$sum = function(...) {
    do.call(paste0, c(list(...), list(collapse = self$.add)))
  }

  return_object(self, "LabelMultiply")
}

#' @export
LabelWrapper = function(open = "(", close = ")") {
  self = Base()
  self$.open = open
  self$.close = close
  self$wrap = function(x) paste(self$.open, x, self$.close, sep = "")
  return_object(self, "LabelWrapper")
}
