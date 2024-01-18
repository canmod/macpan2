#' Symbolizer
#'
#' Convert a numeric function into one that can symbolically
#' manipulate strings as though they were mathematical symbols.
#'
#' Only certain functions are allowed to be utilized in the
#' mathematical function. See the examples for how to get
#' a list of these functions.
#'
#' @param f function that takes numeric arguments and returns
#' a numeric output
#'
#' @examples
#' gg = function(S, I, R, beta) beta * S * I / (S + I + R)
#' gg(1, 0.1, 0, 0.1)
#' symbolizer(gg)(1, 0.1, 0, 0.1)
#' ff = function(x, i, j) x[i, j] / sum(x)
#' ff(matrix(1, 3, 3), 2, 2)
#' symbolizer(ff)("x", 2, 2)
#' gs = symbolizer(gg)
#' adder = symbolizer(function(x, y) (x) + (y))
#' adder(
#'   gs("S_unvax", "I_unvax", "R_unvax", "beta_unvax"),
#'   gs("S_unvax", "I_vax", "R_vax", "beta_vax")
#' )
#' ff = symbolizer(function(x, y, z) {
#'   A = x + y
#'   B = z^2
#'   A / B
#' })
#' ff(1, 2, 3)
#'
#' # allowed functions:
#' ls(environment(ff))
#' @export
symbolizer = function(f) {
  symbolic_arithmetic_env = list2env(
    local({
      `csv` = function(...) do.call(paste, c(list(...), list(sep = ", ")))
      `fwrap` = function(f, x) {
        only_vars = grepl("^[a-zA-Z0-9_]+$", x) # TODO: fix this re
        already_pars = grepl("^\\(.*\\)$", x) # TODO: fix this re to work for cases like "(a - 1) * (b - 2)" -- don't think this will actually work with regex and need recursion
        only_vars = FALSE
        already_pars = FALSE
        if (only_vars | already_pars) {
          # no gratuitous wrapping is necessary
          return(paste(f, x, sep = ""))
        }
        paste(f, "(", x, ")", sep = "")
      }
      `wrap` = function(x) fwrap("", x)
      `bwrap` = function(x, i) paste(x, "[", i, "]", sep = "")
      `binop` = function(op, x, y) wrap(paste(x, y, sep = op))
      list(
          `+` = function(x, y) binop(" + ", x, y),
          `-` = function(x, y) binop(" - ", x, y),
          `*` = function(x, y) binop(" * ", x, y),
          `/` = function(x, y) binop(" / ", x, y),
          `^` = function(x, y) binop(" ^ ", x, y),
          `(` = function(x) wrap(x),
          `c` = function(...) fwrap("c", csv(...)),
          `matrix` = function(x, i, j) fwrap("matrix", csv(x, i, j)),
          `%*%` = function(x, y) binop(" %*% ", x, y),
          `sum` = function(...) fwrap("sum", csv(...)),
          `rep` = function(x, n) fwrap("rep", csv(x, n)),
          `row_sums` = function(x) fwrap("row_sums", x),
          `col_sums` = function(x) fwrap("col_sums", x),
          `[` = function(x, ...) bwrap(x, csv(...))
      )
    })
  )
  environment(f) = symbolic_arithmetic_env
  f
}
