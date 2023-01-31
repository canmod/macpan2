# Overriding Math Functions
#
# Create object with functions that override
#

# FF = function(math_function) {
#   force(math_function)
#   `/` = function(x, y) paste(x, y, sep = " / ")
#   environment(math_function) = environment()
#   math_function
# }
# f = FF(function(x, y) x / y)
# f(1, 2)
# GG = function(math_function) {
#   self = new.env(parent = baseenv())
#   self$symbolic = FF(math_function)
#   self
# }
# g = GG(function(x, y) x / y)
# g$symbolic(1, 2)
#
# FF = function(math_function) {
#   force(math_function)
#   local({
#     wrap = function(x) {
#       force(x)
#       paste("(", x, ")", sep = "")
#     }
#     csv = function(...) {
#       wrap(paste0(as.character(list(...)), collapse = ", "))
#     }
#     is_wrapped = function(x) {
#       force(x)
#       x = str2lang(x)
#       if (!is.symbol(x)) x = x[[1L]]
#       x = as.character(x)
#       x == "("
#     }
#     fwrap = function(f, x) {
#       f = force(f)
#       x = force(x)
#       if (is_wrapped(x)) return(paste(f, x, sep = ""))
#       paste(f, "(", x, ")", sep = "")
#     }
#     bwrap = function(x, i) {
#       x = force(x)
#       i = force(i)
#       paste(x, "[", i, "]", sep = "")
#     }
#     binop = function(op, x, y) {
#       force(x)
#       force(y)
#       force(op)
#       wrap(paste(x, y, sep = op))
#     }
#
#     ## 1. all functions in self take string (i.e. length-1 character vector)
#     ## arguments and return strings
#     ##
#     ## OR
#     ##
#     ## 2. all functions in self take character vector arguments and return
#     ## character vectors
#     ##
#     ## thinking that option #1 is best, because it is easier for me to
#     ## think about scalars and i don't think that it should be an issue
#     ## to package these things up into whatever vector/matrix we want
#     `+` = function(x, y) {
#       force(x)
#       force(y)
#       binop(" + ", x, y)
#     }
#     `-` = function(x, y) binop(" - ", x, y)
#     `*` = function(x, y) binop(" * ", x, y)
#     `/` = function(x, y) {
#       force(x)
#       force(y)
#       binop(" / ", x, y)
#     }
#     `^` = function(x, y) binop(" ^ ", x, y)
#     `(` = function(x) wrap(x)
#     `c` = function(...) fwrap("c", csv(...))
#     `matrix` = function(x, i, j) fwrap("matrix", csv(x, i, j))
#     `%*%` = function(x, y) binop(" %*% ", x, y)
#     `sum` = function(...) fwrap("sum", csv(...))
#     `rep` = function(x, n) fwrap("rep", csv(x, n))
#     `rowSums` = function(x) fwrap("rowSums", x)
#     `colSums` = function(x) fwrap("colSums", x)
#     `[` = function(x, ...) bwrap(x, csv(...))
#     math_function
#   })
# }

# f = FF(function(x, y) x / y)
# f(1, 2)

# csv(letters)
# as.character(unlist(list(c("a", "b"), "c")))

# SymbolicMath = function() {
#   self = Base(baseenv())
#   self$wrap = function(x) {
#     force(x)
#     paste("(", x, ")", sep = "")
#   }
#   self$csv = function(...) {
#     self$wrap(paste0(as.character(list(...)), collapse = ", "))
#   }
#   self$is_wrapped = function(x) {
#     force(x)
#     x = str2lang(x)
#     if (!is.symbol(x)) x = x[[1L]]
#     x = as.character(x)
#     x == "("
#   }
#   self$fwrap = function(f, x) {
#     f = force(f)
#     x = force(x)
#     if (self$is_wrapped(x)) return(paste(f, x, sep = ""))
#     paste(f, "(", x, ")", sep = "")
#   }
#   self$bwrap = function(x, i) {
#     x = force(x)
#     i = force(i)
#     paste(x, "[", i, "]", sep = "")
#   }
#   self$binop = function(op, x, y) {
#     force(x)
#     force(y)
#     force(op)
#     self$wrap(paste(x, y, sep = op))
#   }
#
#   ## 1. all functions in self take string (i.e. length-1 character vector)
#   ## arguments and return strings
#   ##
#   ## OR
#   ##
#   ## 2. all functions in self take character vector arguments and return
#   ## character vectors
#   ##
#   ## thinking that option #1 is best, because it is easier for me to
#   ## think about scalars and i don't think that it should be an issue
#   ## to package these things up into whatever vector/matrix we want
#   self$`+` = function(x, y) {
#     force(x)
#     force(y)
#     self$binop(" + ", x, y)
#   }
#   self$`-` = function(x, y) self$binop(" - ", x, y)
#   self$`*` = function(x, y) self$binop(" * ", x, y)
#   self$`/` = function(x, y) {
#     force(x)
#     force(y)
#     self$binop(" / ", x, y)
#   }
#   self$`^` = function(x, y) self$binop(" ^ ", x, y)
#   self$`(` = function(x) self$wrap(x)
#   self$`c` = function(...) self$fwrap("c", self$csv(...))
#   self$`matrix` = function(x, i, j) self$fwrap("matrix", self$csv(x, i, j))
#   self$`%*%` = function(x, y) self$binop(" %*% ", x, y)
#   self$`sum` = function(...) self$fwrap("sum", self$csv(...))
#   self$`rep` = function(x, n) self$fwrap("rep", self$csv(x, n))
#   self$`rowSums` = function(x) self$fwrap("rowSums", x)
#   self$`colSums` = function(x) self$fwrap("colSums", x)
#   self$`[` = function(x, ...) self$bwrap(x, self$csv(...))
#   return_object(self, "SymbolicMath")
# }

SymbolicMath = function() {
  self = list()
  self$wrap = function(x) {
    force(x)
    paste("(", x, ")", sep = "")
  }
  self$csv = function(...) {
    paste0(as.character(list(...)), collapse = ", ")
  }
  self$is_wrapped = function(x) {
    force(x)
    x = try(str2lang(x), silent = TRUE)
    if (inherits(x, "try-error")) return(FALSE)
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
  self$`exp` = function(x) self$fwrap("exp", x)
  self$`log` = function(x) self$fwrap("log", x)
  self$`(` = function(x) self$wrap(x)
  self$`c` = function(...) {
    # browser()
    self$fwrap("c", self$csv(...))
  }
  self$`matrix` = function(x, i, j) self$fwrap("matrix", self$csv(x, i, j))
  self$`%*%` = function(x, y) self$binop(" %*% ", x, y)
  self$`sum` = function(...) self$fwrap("sum", self$csv(...))
  self$`rep` = function(x, n) self$fwrap("rep", self$csv(x, n))
  self$`rowSums` = function(x) self$fwrap("rowSums", x)
  self$`colSums` = function(x) self$fwrap("colSums", x)
  self$`groupSums` = function(x, f, n) self$fwrap("groupSums", self$csv(x, f, n))
  self$`[` = function(x, ...) self$bwrap(x, self$csv(...))
  self$`block` = function(x, i, j, n, m) self$fwrap("block", self$csv(x, i, j, n, m))
  self$`t` = function(x) self$fwrap("t", x)
  self$`rbind_time` = function(x, lag, t_min) self$fwrap("rbind_time", self$csv(x, lag, t_min))
  self$`rbind_lag` = function(x, lag, t_min) self$fwrap("rbind_lag", self$csv(x, lag, t_min))
  self$':' = function(from, to) self$binop(" : ", from, to)
  self$`seq` = function(from, length, by) self$fwrap("seq", self$csv(from, length, by))
  self$`convolution` = function(x, k) self$fwrap("convolution", self$csv(x, k))
  self$`assign` = function(x, i, j, v) self$fwrap("assign", self$csv(x, i, j, v))
  self$`unpack` = function(x, ...) self$fwrap("unpack", self$csv(x, ...))
  self$`clamp` = function(x) self$fwrap("clamp", x)
  self$`dpois` = function(x, y) self$fwrap("dpois", self$csv(x, y))
  self$`dnbinom` = function(x, y, z) self$fwrap("dnbinom", self$csv(x, y, z))
  self$`dnorm` = function(x, y, z) self$fwrap("dnorm", self$csv(x, y, z))
  self$`rpois` = function(x) self$fwrap("rpois", x)
  self$`rnbinom` = function(x, y) self$fwrap("rnbinom", self$csv(x, y))
  self$`rnorm` = function(x, y) self$fwrap("rnorm", self$csv(x, y))
  list2env(self)
  #return_object(self, "SymbolicMath")
}

NumericMath = function() {
  self = Base(baseenv())
  #self$`*` = BinaryOperator(`*`)
  return_object(self, "NumericMath")
}

#' @importFrom oor return_facade

MathOverrider = function(math_function, function_environment) {
  self = Base()
  #self$evaluate = math_function
  self$math_function = math_function
  self$evaluate = function(...) {
    # browser()
    l = list(...)
    for (i in seq_along(l)) {
      force(l[[i]])
    }
    do.call(self$math_function, l)
  }
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
#' @param argument_strings Character vector of the names of the arguments of
#' the mathematical expression.
#' @param expression_string String representing the expression, referring only to
#' the names of the arguments and the names of the following functions:
#' r allowed_math_functions.
#' @param include_dots Does the expression expect three dots?
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
  self$arguments = force(names(formals(args(math_function))))
  self$numeric = MathOverrider(math_function, NumericMath())
  self$symbolic = MathOverrider(math_function, SymbolicMath())
  #environment(self$symbolic$evaluate) = list2env(as.list(environment()))
  self$string = do.call(
    self$symbolic$evaluate,
    as.list(self$arguments)
  )
  return_object(self, "MathExpression")
}

#' @rdname MathExpression
#' @export
MathExpressionFromStrings = function(expression_string
    , argument_strings = character(0L)
    , include_dots = FALSE
  ) {
  self = Base()
  # browser()
  math_function = function() {}
  if (include_dots) argument_strings = c(argument_strings, "...")
  blank_args = rep(list(quote(expr = )), length(argument_strings))
  formals(math_function) = setNames(blank_args, argument_strings)
  body(math_function) = parse(text = expression_string)
  self$arguments = argument_strings
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

#' @export
print.MathExpression = function(x, ...) {
  cat("Math expression given by the following function:\n")
  print(x$symbolic$math_function)
}

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


# MathExpressionFromStrings = function(arguments, string) {
#   symbolic_math = SymbolicMath()
#   numeric_math = NumericMath()
#   self = Unclean()
#
#   self$string = valid$char1$assert(string)
#   self$arguments = valid$char$assert(arguments)
#
#   math_function = function() {}
#   blank_args = rep(list(quote(expr = )), length(arguments))
#   formals(math_function) = setNames(blank_args, arguments)
#   body(math_function) = parse(text = string)
#   self$numeric_function = math_function
#   self$symbolic_function = math_function
#   environment(self$numeric_function) = numeric_overider
#   environment(self$symbolic_function) = symbolic_overider
#
#   return_object(self, "MathExpression")
# }
