#' Transform
#'
#' @param variable Character string giving the name of a variable in the model.
#' @param default Default value for the untransformed variable. If `NULL`
#' (the default) this value is taken from the initial value in the model
#' containing the transformation.
#' @param trans_variable Character string to use as the name of the transformed
#' version of the `variable`.
#'
#' @export
Transform <- function(variable, default = NULL, trans_variable = variable) {
  self = Base()
  self$variable = variable
  self$default = default
  self$trans_variable = trans_variable
  self$trans_two_sided_formula = function() {
    two_sided(
      self$trans_variable,
      self$trans$symbolic$evaluate(self$variable)
    )
  }
  self$trans_one_sided_formula = function() {
    one_sided(self$trans$symbolic$evaluate(self$variable))
  }
  self$inverse_two_sided_formula = function() {
    two_sided(
      self$variable,
      self$inverse$symbolic$evaluate(self$trans_variable)
    )
  }
  self$inverse_one_sided_formula = function() {
    one_sided(self$inverse$symbolic$evaluate(self$trans_variable))
  }
  self$trans_engine_eval = function(value) {
    do.call(
      engine_eval,
      setNames(
        list(self$trans_one_sided_formula(), value),
        c(".expr", self$variable)
      )
    )
  }
  self$inverse_engine_eval = function(trans_value) {
    do.call(
      engine_eval,
      setNames(
        list(self$inverse_one_sided_formula(), trans_value),
        c(".expr", self$trans_variable)
      )
    )
  }
  return_object(self, "Transform")
}

#' @describeIn Transform Identity transformation.
Identity = function(variable, default = NULL, trans_variable = variable) {
  self = Transform(variable, default, trans_variable)
  self$trans = MathExpressionFromStrings("x", "x")
  self$inverse = MathExpressionFromStrings("x", "x")
  self$prototype = MethodPrototype(y ~ x, c("x", "y"), character())
  return_object(self, "Identity")
}

#' @describeIn Transform Log transformation.
#' @export
Log = function(variable, default = NULL, trans_variable = sprintf("log_%s", variable)) {
  self = Transform(variable, default, trans_variable)
  self$trans = MathExpressionFromStrings("log(x)", "x")
  self$inverse = MathExpressionFromStrings("exp(x)", "x")
  self$prototype = MethodPrototype(y ~ log(x), c("x", "y"), character())
  return_object(self, "Log")
}

#' @describeIn Transform Logit transformation.
#' @export
Logit = function(variable, default = NULL, trans_variable = sprintf("logit_%s", variable)) {
  self = Transform(variable, default, trans_variable)
  self$trans = MathExpressionFromStrings("log(x) - log(1 - x)", "x")
  self$inverse = MathExpressionFromStrings("1 / (1 + exp(-x))", "x")
  self$prototype = MethodPrototype(y ~ logit(x), c("x", "y"), character())
  return_object(self, "Logit")
}


find_trans = function(formula) {
  trans_variable = trans_lhs_var(formula)
  variable = trans_rhs_var(formula)
  all_trans_cls = list(Identity, Log, Logit)
  for (trans_cls in all_trans_cls) {
    trans_obj = trans_cls(variable, trans_variable = trans_variable)
    if (trans_obj$prototype$consistent(formula)) {
      return(trans_obj)
    }
  }
  Identity(variable, trans_variable = trans_variable)
}
