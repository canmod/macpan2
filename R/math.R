# Symbolic Math
#
# Create object with functions that overrides
#

# csv(letters)
# as.character(unlist(list(c("a", "b"), "c")))

SymbolicMath = function() {
  self = Base(baseenv())
  self$wrap = function(x) {
    force(x)
    paste("(", x, ")", sep = "")
  }
  self$csv = function(...) {
    self$wrap(paste0(as.character(list(...)), collapse = ", "))
  }
  self$is_wrapped = function(x) {
    force(x)
    x = str2lang(x)
    if (!is.symbol(x)) x = x[[1L]]
    x = as.character(x)
    x == "("
  }
  self$fwrap = function(f, x) {
    f = force(f)
    x = force(x)
    if (self$is_wrapped(x)) return(paste(f, x, sep = ""))
    paste(f, "(", x, ")", sep = "")
  }
  self$bwrap = function(x, i) {
    x = force(x)
    i = force(i)
    paste(x, "[", i, "]", sep = "")
  }
  self$binop = function(op, x, y) {
    force(x)
    force(y)
    force(op)
    self$wrap(paste(x, y, sep = op))
  }

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
  self$`+` = function(x, y) {
    force(x)
    force(y)
    self$binop(" + ", x, y)
  }
  self$`-` = function(x, y) self$binop(" - ", x, y)
  self$`*` = function(x, y) self$binop(" * ", x, y)
  self$`/` = function(x, y) {
    force(x)
    force(y)
    self$binop(" / ", x, y)
  }
  self$`^` = function(x, y) self$binop(" ^ ", x, y)
  self$`(` = function(x) self$wrap(x)
  self$`c` = function(...) self$fwrap("c", self$csv(...))
  self$`matrix` = function(x, i, j) self$fwrap("matrix", self$csv(x, i, j))
  self$`%*%` = function(x, y) self$binop(" %*% ", x, y)
  self$`sum` = function(...) self$fwrap("sum", self$csv(...))
  self$`rep` = function(x, n) self$fwrap("rep", self$csv(x, n))
  self$`rowSums` = function(x) self$fwrap("rowSums", x)
  self$`colSums` = function(x) self$fwrap("colSums", x)
  self$`[` = function(x, ...) self$bwrap(x, self$csv(...))
  return_object(self, "SymbolicMath")
}

NumericMath = function() {
  self = Base(baseenv())
  #self$`*` = BinaryOperator(`*`)
  return_object(self, "NumericMath")
}

#' @importFrom oor return_facade

MathOverrider = function(math_function, function_environment) {
  self = Base()
  self$evaluate = math_function
  return_facade(self, function_environment, "MathOverrider")
}

#' Math Expression
#'
#' Construct objects for representing mathematical expressions.
#' These objects allow for both numeric and symbolic
#' evaluations of the expressions. Only the following functions
#' can be used to construct mathematical expressions:
#' r allowed_math_functions. There are two alternative
#' constructors. \code{MathExpressionFromFunc} takes a numerical
#' function and returns a \code{MathExpression} object.
#' \code{MathExpressionFromString} takes a character vector giving
#' the names of the arguments and a string representing the expression,
#' and returns a \code{MathExpression} object.
#'
#' @param math_function Function that takes any number of named
#' \code{\link{numeric}} arguments and returns a \code{\link{numeric}}
#' object. The only functions that can be used in this function include
#' r allowed_math_functions.
#' @param arguments Character vector of the names of the arguments of
#' the mathematical expression.
#' @param string String representing the expression, referring only to
#' the names of the arguments and the names of the following functions:
#' r allowed_math_functions.
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
  self = Base()
  self$arguments = names(formals(args(math_function)))
  self$numeric = MathOverrider(math_function, NumericMath())
  self$symbolic = MathOverrider(math_function, SymbolicMath())
  self$string = do.call(
    self$symbolic$evaluate,
    as.list(self$arguments)
  )
  return_object(self, "MathExpression")
}
# xx = MathExpression(function(x, y) {z = x / y; z^2})
# xx$arguments
# xx$string
# xx$numeric$evaluate(1, 2)
# xx$symbolic$evaluate(1, 2)
#
# xx = MathOverrider(function(x, y) x / y, macpan2:::NumericMath())
# xx$math_function(1, 2)

# symbolic_math = SymbolicMath()
# numeric_math = NumericMath()
# allowed_math_functions = paste0(
#     paste("\\code{\\link{", names(numeric_math), "}}", sep = ""),
#   collapse = ", "
# )
MathExpression = function(math_function) {
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
