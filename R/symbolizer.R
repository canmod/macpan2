#' Symbolizer
#'
#' Convert a numeric function into one that can symbolically
#' manipulate strings as though they were mathemtical symbols.
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
      `wrap` = function(f, x) paste(f, "(", x, ")", sep = "")
      `bwrap` = function(x, i) paste("x", "[", i, "]", sep = "")
      `binop` = function(f, x, y) paste(x, y, sep = f)
      list(
          `+` = function(x, y) binop(" + ", x, y),
          `-` = function(x, y) binop(" - ", x, y),
          `*` = function(x, y) binop(" * ", x, y),
          `/` = function(x, y) binop(" / ", x, y),
          `^` = function(x, y) binop(" ^ ", x, y),
          `(` = function(x) wrap("", x),
          `c` = function(...) wrap("c", csv(...)),
          `matrix` = function(x, i, j) wrap("matrix", csv(x, i, j)),
          `%*%` = function(x, y) binop(" %*% ", x, y),
          `sum` = function(...) wrap("sum", csv(...)),
          `rep` = function(x, n) wrap("rep", csv(x, n)),
          `rowSums` = function(x) wrap("rowSums", x),
          `colSums` = function(x) wrap("colSums", x),
          `[` = function(x, i, j) bwrap(x, csv(i, j))
      )
    })
  )
  environment(f) = symbolic_arithmetic_env
  f
}

