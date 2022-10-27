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
          `rowSums` = function(x) fwrap("rowSums", x),
          `colSums` = function(x) fwrap("colSums", x),
          `[` = function(x, ...) bwrap(x, csv(...))
      )
    })
  )
  environment(f) = symbolic_arithmetic_env
  f
}


# xx = MathExpressionConstructors()$from_function(function(x, y) x + y)
# xx = MathExpressionConstructors()$from_strings(c("x", "y"), "x + y")
# xx$symbolic_function(1, 2)
# xx$numeric_function(1, 2)
# xx$arguments
# xx$string
#
#
# foi_base = MathExpression(function(S, I, R, beta) S * I * beta / (S + I + R))
# foi_summer = MathExpressionConstructors()$from_strings("foi_vec", "sum(foi_vec)")
# foi_summer$symbolic_function(
#   foi_base$symbolic_function("S_unvax", "I_unvax", "R_unvax", "beta_unvax"),
#   foi_base$symbolic_function("S_unvax", "I_vax", "R_vax", "beta_vax")
# )

# FoiSymbolic = function() {
#   self = SymbolicMath()
#   self$foi = foi_base$symbolic_function
#   return_object(self, "FoiSymbolic")
# }
# FoiNumeric = function() {
#   self = SymbolicMath()
#   self$foi = foi_base$numeric_function
#   return_object(self, "FoiNumeric")
# }
# foi = MathExpression(
#   function(S_unvax, I_unvax, R_unvax, beta_unvax, S_vax, I_vax, R_vax, beta_vax) {
#     foi(S_unvax, I_unvax, R_unvax, beta_unvax) + foi(S_unvax, I_vax, R_vax, beta_vax)
#   },
#   FoiSymbolic(),
#   FoiNumeric()
# )
# foi$symbolic_function(1, 0.1, 0, 0.2, 1, 0.2, 0, 0.5)
#
# foi$numeric_function(0.99, 0.01, 0, 0.2)
# foi$symbolic_function(0.99, 0.01, 0, 0.2)
#
# FoiCombiner = function(foi_1, foi_2) {
#   self = MathExpression(
#     math_function = function(foi_1, foi_2) foi + foi_2),
#
#   )
# }

# foi_unvax = function(S_unvax, I_unvax, R_unvax, beta_unvax, S_vax, I_vax, R_vax, beta_vax) {
#   unvax = foi$symbolic_function(S_unvax, I_unvax, R_unvax, beta_unvax)
#   vax = foi$symbolic_function(S_unvax, I_vax, R_vax, beta_vax)
#   unvax + vax
# }
# foi_unvax("S_unvax", "I_unvax", "R_unvax", "beta_unvax", "S_vax", "I_vax", "R_vax", "beta_vax")
# xx = MathExpression(foi_unvax)
#
# xx = MathExpression(function(AA, BB) {AA + BB})
# yy = MathExpression(function(x, y) {
#   z = x - y
#   A = 1 + x + x^2
#   A * (z * z - x) / 36
# })
# yy$string
# yy$numeric_function(2, 1)
# yy$symbolic_function("S", "I")
#
# zz = MathExpression(function(x) {
#   a = (1 + x + x^2)
#   a * 2
# })
# zz$string
# zz$numeric_function(2)
# zz$symbolic_function("S")
#
# ww = MathExpression(function(x, y) {
#   A = matrix(c(x, x[1]), 2, 2)
#   B = matrix(c(y, y[1]), 2, 2)
#   rowSums(A %*% B)
# })
# ww$symbolic_function("x_unvax", "y_unvax")
# ww$numeric_function(1:3, 1:3)
# ww$arguments
