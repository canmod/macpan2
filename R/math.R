# Symbolic Math
#
# Create object with functions that overides
#

# csv(letters)
# as.character(unlist(list(c("a", "b"), "c")))

SymbolicMath = function() {
  self = Unclean()
  `{` = `{`
  `=` = `=`
  `$` = `$`
  wrap = function(x) paste("(", x, ")", sep = "")
  csv = function(...) {
    wrap(paste0(as.character(list(...)), collapse = ", "))
  }
  is_wrapped = function(x) {
    #print(x)
    x = str2lang(x)
    #print(x)
    if (!is.symbol(x)) x = x[[1L]]
    #print(x)
    x = as.character(x)
    #print(x)
    x == "("
  }
  fwrap = function(f, x) {

    if (is_wrapped(x)) return(paste(f, x, sep = ""))
    paste(f, "(", x, ")", sep = "")
  }
  bwrap = function(x, i) paste(x, "[", i, "]", sep = "")
  binop = function(op, x, y) wrap(paste(x, y, sep = op))

  ## 1. all functions in self take string (i.e. length-1 character vector)
  ## arguments and return strings
  ##
  ## OR
  ##
  ## 2. all functions in self take character vector arguments and return
  ## character vectors
  ##
  ## thinking that option #1 is best, because it is easier for me to
  ## think about scalars and i don't think that it should be an issue
  ## to package these things up into whatever vector/matrix we want
  self$`+` = function(x, y) binop(" + ", x, y)
  self$`-` = function(x, y) binop(" - ", x, y)
  self$`*` = function(x, y) binop(" * ", x, y)
  self$`/` = function(x, y) binop(" / ", x, y)
  self$`^` = function(x, y) binop(" ^ ", x, y)
  self$`(` = function(x) wrap(x)
  self$`c` = function(...) fwrap("c", csv(...))
  self$`matrix` = function(x, i, j) fwrap("matrix", csv(x, i, j))
  self$`%*%` = function(x, y) binop(" %*% ", x, y)
  self$`sum` = function(...) fwrap("sum", csv(...))
  self$`rep` = function(x, n) fwrap("rep", csv(x, n))
  self$`rowSums` = function(x) fwrap("rowSums", x)
  self$`colSums` = function(x) fwrap("colSums", x)
  self$`[` = function(x, ...) bwrap(x, csv(...))
  return_object(self, "SymbolicMath")
}

NumericMath = function() {
  self = Unclean()
  `{` = `{`
  `=` = `=`
  `$` = `$`
  self$`+` = `+`
  self$`-` = `-`
  self$`*` = `*`  ## TODO: define this so that it works like the C++ side
  self$`/` = `/`
  self$`^` = `^`
  self$`(` = `(`
  self$`c` = `c`
  self$`matrix` = `matrix`
  self$`%*%` = `%*%`
  self$`sum` = `sum`
  self$`rep` = `rep`
  self$`rowSums` = `rowSums`
  self$`colSums` = `colSums`
  self$`[` = `[`
  return_object(self, "NumericMath")
}

symbolic_math = SymbolicMath()
numeric_math = NumericMath()
allowed_math_functions = paste0(
    paste("\\code{\\link{", names(numeric_math), "}}", sep = ""),
  collapse = ", "
)

#' Math Expression
#'
#' Construct objects for representing mathematical expressions.
#' These objects allow for both numeric and symbolic
#' evaluations of the expressions. Only the following functions
#' can be used to construct mathematical expressions:
#' `r allowed_math_functions`. There are two alternative
#' constructors. \code{MathExpressionFromFunc} takes a numerical
#' function and returns a \code{MathExpression} object.
#' \code{MathExpressionFromString} takes a character vector giving
#' the names of the arguments and a string representing the expression,
#' and returns a \code{MathExpression} object.
#'
#' @param math_function Function that takes any number of named
#' \code{\link{numeric}} arguments and returns a \code{\link{numeric}}
#' object. The only functions that can be used in this function include
#' `r allowed_math_functions`.
#' @param arguments Character vector of the names of the arguments of
#' the mathematical expression.
#' @param string String representing the expression, referring only to
#' the names of the arguments and the names of the following functions:
#' `r allowed_math_functions`.
#'
#' @field string String representing the mathematical expression.
#' @field arguments Character vector of the names of the arguments of
#' the mathematical expression.
#'
#'
#' @section Methods:
#'
#' * `$numeric_function()`: Function to numerically evaluate the mathematical
#' expression.
#' * `$symbolic_function()`: Function to symbolically evaluate the
#' mathematical expression.
#'
#' ## Method Arguments
#'
#' The argument signature depends on the expression itself,
#' and can be found in the \code{arguments} field.
#'
#' @name MathExpression
#' @export
MathExpressionFromFunc = function(math_function) {
  symbolic_math = SymbolicMath()
  numeric_math = NumericMath()

  self = Unclean()
  valid$func$check(math_function)
  self$numeric_function = math_function
  self$symbolic_function = math_function

  self$arguments = names(formals(math_function))
  environment(self$numeric_function) = numeric_math
  environment(self$symbolic_function) = symbolic_math
  self$string = do.call(
    self$symbolic_function,
    as.list(self$arguments)
  )

  return_object(self, "MathExpression")
}

#' @rdname MathExpression
#' @export
MathExpressionFromStrings = function(arguments, string) {
  symbolic_math = SymbolicMath()
  numeric_math = NumericMath()
  self = Unclean()

  self$string = valid$char1$assert(string)
  self$arguments = valid$char$assert(arguments)

  math_function = function() {}
  blank_args = rep(list(quote(expr = )), length(arguments))
  formals(math_function) = setNames(blank_args, arguments)
  body(math_function) = parse(text = string)
  self$numeric_function = math_function
  self$symbolic_function = math_function
  environment(self$numeric_function) = numeric_overider
  environment(self$symbolic_function) = symbolic_overider

  return_object(self, "MathExpression")
}
