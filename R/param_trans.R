#' Transform
#'
#' @param variable Character string giving a variable in the model.
#' @param default Default value for the untransformed variable. If `NULL`
#' (the default) this value is taken from the initial value in the model
#' containing the transformation.
#' @param prefix Character string to use to distinguish the transformed
#' version of the `variable`.
#' @param default (??)
#'
#' @export
Transform <- function(variable, default = NULL, prefix = "") {
  self = Base()
  self$variable = variable
  self$default = default
  self$prefix = prefix
  self$trans_variable = sprintf("%s_%s", self$prefix, self$variable)
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
        c("e", self$variable)
      )
    )
  }
  self$inverse_engine_eval = function(trans_value) {
    do.call(
      engine_eval,
      setNames(
        list(self$inverse_one_sided_formula(), trans_value),
        c("e", self$trans_variable)
      )
    )
  }
  return_object(self, "Transform")
}

#' @describeIn Transform Log transformation
#' @export
Log = function(variable, default = NULL) {
  self = Transform(variable, default, "log")
  self$trans = MathExpressionFromStrings("log(x)", "x")
  self$inverse = MathExpressionFromStrings("exp(x)", "x")
  return_object(self, "Log")
}

#' @describeIn Transform Logit transformation
#' @export
Logit = function(variable, default = NULL) {
  self = Transform(variable, default, "logit")
  self$trans = MathExpressionFromStrings("log(x) - log(1 - x)", "x")
  self$inverse = MathExpressionFromStrings("1 / (1 + exp(-x))", "x")
  return_object(self, "Logit")
}
