library(oor)

## two class types:
##   1. ChangeModel (e.g. set of flows)
##   2. UpdateMethod (e.g. Euler, RK4, EulerMultinomial, TimeDerivative)
##
## one data structure type:
##   1. ChangeComponent (e.g. Flow, Birth, ...)

ChangeModel = function() {
  self = Base()
  self$per_capita_rate_exprs = function() list()
  
  ## one element per state matrix
  ## list names: state matrix names
  ## list elements: expression giving absolute rate of change of that 
  ##                state matrix per unit time
  self$absolute_rate_exprs = function() list()
  
  self$state_matrices = function() character()
  #self$absolute
  
  self$default = function() list()
  self$integers = function() list()
  return_object(self, "ChangeModel")
}

UpdateMethod = function() {
  self = Base()
  self$before = function() list()
  self$during = function() list()
  self$after = function() list()
  return_object(self, "UpdateMethod")
}

ChangeComponent = function() {
  self = Base()
  return_object(self, "ChangeComponent")
}



EulerUpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model
  self$during = function() {
    self$change_model
  }
  return_object(self, "EulerUpdateMethod")
}



FormulaListGenerator = function() {
  self = Base()
  self$formula_list = function() list()
  return_object(self, "FormulaListGenerator")
}
as.list.FormulaListGenerator = function(x, ...) x$formula_list()

## developers should call this function whenever formula
## lists or formula list generators are used, not when saved.
## the idea is to maintain structure in data but 
assert_formula_list = function(x) {
  x = as.list(x)
  bad = !vapply(x, inherits, logical(1L), "formula")
  if (any(bad)) stop("Not all list elements are formulas.")
  x
}

AllStateUpdateGenerator = function(update_list = list()) {
  self = FormulaListGenerator()
  self$update_list = update_list
  self$formula_list = function() {
    method_apply(self$update_list, "before_loop") |> flatten()
    method_apply(self$update_list, "before_update") |> flatten()
    method_apply(self$update_list, "during_update") |> flatten()
  }
  return_object(self, "AllStateUpdateGenerator")
}




AllStateGradientGenerator = function(update_list = list()){
  self = FormulaListGenerator()
  self$update_list = update_list
  self$formula_list = function() {
    
  }
}



StateChangeGenerator = function() {
  self = Base()
  self$deterministic_change_exprs = function() list()
  self$stochastic_change_exprs = function() list()
  return_object(self, "StateUpdateGenerator")
}
recycle_to_match = function(x, ...) {
  if (length(x) == 1L) {
    lens = list(...) |> vapply(length, integer(1L)) |> unique()
    lens = lens[lens > 1]
    if (length(lens) == 0L) return(x)
    if (length(lens) != 1L) stop("Inconsistent recycling request")
    x = rep(x, times = lens)
  }
  x
}
# char_expr_to_name = function(x) {
#   (x
#    |> gsub(pattern = "[^a-zA-Z0-9]+", replacement = " ")
#    |> make.names()
#   )
# }

#' @param from Character vector of `from` states
#' @param to Character vector of `to` states
#' @param rates List of two-sided formulas for computing the per-capita 
#' rate of flow from `from` to `to` in one time unit. Could also be a 
#' character vector giving expressions that would evaluate to the per-capita
#' rate.
TMBFlow = function(from, to, rates) {
  self = StateChangeGenerator()
  stopifnot(length(from) == 1L)
  stopifnot(is.character(from))
  stopifnot(is.list(rates) | is.character(rates) | inherits(rates, "formula"))
  stopifnot(is.character(to))
  if (inherits(rates, "formula")) rates = list(rates)
  self$from = from
  self$to = to
  self$rates = rates
  
  ## methods that return from/to/rate columns in a potential
  ## data frame describing per-capita flows among states
  self$from_column = function() {
    recycle_to_match(self$from, self$to, self$rates)
  }
  self$to_column = function() {
    recycle_to_match(self$to, self$from, self$rates)
  }
  self$rate_expr_column = function() {
    recycle_to_match(self$rates, self$from, self$to)
  }
  self$per_capita_column = function() {
    if (is.character(self$rates)) return(self$rate_expr_column())
    pc = recycle_to_match(
        vapply(self$rates, lhs_char, character(1L))
      , self$from, self$to
    )
    sprintf("(%s)", pc)
  }
  self$deterministic_change_column = function() {
    sprintf("%s * %s", self$per_capita_column(), self$from_column())
  }
  self$stochastic_change_column = function() {
    stop("not implemented yet")
  }
  
  self$preliminary_exprs = function() {
    if (is.character(self$rates)) return(list())
    self$rates
  }
  self$deterministic_change_exprs = function() {
    ## what if `from` and `to` are vectors!? wait ... maybe it is fine?
    exprs = self$deterministic_change_column()
    list(
        positive = setNames(exprs, self$to_column())
      , negative = setNames(exprs, self$from_column())
    )
  }
  self$stochastic_change_exprs = function() {
    exprs = self$stochastic_change_column()
    list(
        positive = setNames(exprs, self$to_column())
      , negative = setNames(exprs, self$from_column())
    )
  }
  return_object(self, "TMBFlow")
}

#debug(xx$deterministic_change_exprs)

xx = TMBFlow("S", "E", list(lambda ~ beta * I / N))
xx = TMBFlow("S", "E", lambda ~ beta * I / N)
xx = TMBFlow("S", "E", "beta * I / N")
xx$preliminary_exprs()
xx$deterministic_change_column()
xx$deterministic_change_exprs()
