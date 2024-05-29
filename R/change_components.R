## ChangeComponent -- functions for producing the change_frame and flow_frame,
## which describe model dynamics, and also other associated formulas
ChangeComponent = function() {
  self = Base()
  
  ## column - state: state variable being changed (i.e. updated at each step)
  ## column - change: signed absolute flow rates (variables or expressions that 
  ## don't involve any state variables)
  ## example:
  ## state, change
  ## S,     -infection
  ## I,     +infection
  ## I,     -recovery
  ## R,     +recovery
  self$change_frame = function() empty_frame("state", "change")
  
  ## column - size: variable (often a state variable or function of
  ## state variables) that gives the size of the population being drawn
  ## from in a flow (e.g. S is the size of an infection flow).
  ## column - change: unsigned absolute flow rates.
  ## column - rate: per-capita flow rates (variables or expresions that 
  ## sometimes involve state variables).
  ## example:
  ## size, change,    rate
  ## S,    infection, beta * I / N
  ## I,    recovery,  gamma
  ## N,    birth,     mu
  self$flow_frame = function() empty_frame("size", "change", "rate")
  
  self$other_generated_formulas = function() list()
  
  self$user_formulas = function() list()
  
  self$string = function() "Abstract change component"
  
  return_object(self, "ChangeComponent")
}

ChangeComponentGlobalizable = function() {
  self = ChangeComponent()
  self$.name_map = list()
  self$name_map = function() self$.name_map
  self$local_names = function() names(self$.name_map)
  self$initialize_name_map = function(local_names) {
    self$.name_map = setNames(as.list(local_names), local_names)
  }
  self$update_name_map = function(existing_global_names) {
    self$.name_map = map_names(existing_global_names, self$local_names())
  }
  return_object(self, "ChangeComponentGlobalization")
}

handle_rate_args = function(rate, abs_rate = NULL) {
  if (!is_two_sided(rate)) {
    if (is_one_sided(rate)) rate = rhs_char(rate)
    rate = two_sided(abs_rate, rate)
  }
  return(rate)
}


PerCapitaFlow = function(from, to, rate, call_string) {
  self = ChangeComponent()
  self$from = from
  self$to = to
  self$rate = rate
  self$call_string = call_string
  self$change_frame = function() {
    data.frame(
        state = c(self$from, self$to)
      , change = sprintf("%s%s", c("-", "+"), lhs_char(self$rate))
    )
  }
  self$flow_frame = function() {
    data.frame(
        size = self$from
      , change = lhs_char(self$rate)
      , rate = rhs_char(self$rate)
    )
  }
  self$string = function() self$call_string
  return_object(self, "PerCapitaFlow")
}

PerCapitaInflow = function(from, to, rate, call_string) {
  self = PerCapitaFlow(from, to, rate, call_string)
  self$change_frame = function() {
    data.frame(
        state = self$to
      , change = sprintf("%s%s", "+", lhs_char(self$rate))
    )
  }
  return_object(self, "PerCapitaInflow")
}


##' Formula
##' 
##' Wrap a two-sided formula so that it can be used as a change component.
##' Developer use only.
##' 
##' @noRd
Formula = function(formula) {
  if (is_one_sided(formula)) stop("Raw R formulas in an expression list must be two-sided")
  self = ChangeComponent()
  self$formula = formula
  self$user_formulas = function() list(self$formula)
  return_object(self, "Formula")
}

FormulaList = function(formulas) {
  one_sided = vapply(formulas, is_one_sided, logical(1L))
  if (all(one_sided)) stop("Raw R formulas in an expression list must be two-sided")
  self = ChangeComponent()
  self$formulas = formulas
  self$user_formulas = function() as.list(self$formulas)
}

mp_sum = function(summand, sum = sprintf("%s_sum", summand)) {
  formula = macpan2:::two_sided(sum, sprintf("sum(%s)", summand))
  macpan2:::Formula(formula)
}

FormulaHelper = function(local_names = character()) {
  self = ChangeComponentGlobalizable()
  self$initialize_name_map(local_names)
  self$user_formulas = function() list()
  return_object(self, "FormulaHelper")
}
FormulaListHelper = function(local_names = character()) {
  self = ChangeComponentGlobalizable()
  self$initialize_name_map(local_names)
  self$user_formulas = function() list()
  return_object(self, "FormulaListHelper")
}

ConvolutedScaler = function(var, var_scaled) {
  self = FormulaListHelper(c("mean", "sd"))
  self$var = var
  self$var_scaled = var_scaled
  self$other_generated_formulas = function() {
    map = self$name_map()
    list(
        two_sided(map$mean, sprintf("mean(%s)", self$var))
      , two_sided(map$sd, sprintf("sd(%s)", self$var))
      , two_sided(self$var_scaled, sprintf("(%s - %s)/%s", self$var, map$mean, map$sd))
    )
  }
  return_object(self, "ConvolutedScaler")
}

GammaConvolution = function(variable, length, height, mean, cv) {
  self = ChangeComponent()
  self$variable = variable
  self$height = height
  self$length = length
  self$mean = mean
  self$cv = cv
  
  length = 4 # length of kernel
  n = 6  # number of time steps
  c_delay_cv = 0.25
  c_delay_mean = 11
  c_prop = 0.1
  gamma_shape = 1 / (c_delay_cv^2)
  gamma_scale = c_delay_mean * c_delay_cv^2
  gamma = pgamma(1:(length + 1), gamma_shape, scale = gamma_scale)
  delta = gamma[2:(length + 1)] - gamma[1:(length)]
  kappa = c_prop * delta / sum(delta)
  expr2 = list(
      X ~ X + X/2
    , gamma ~ pgamma(1:(length+1), gamma_shape, gamma_scale)
    , delta ~ gamma[1:length] - gamma[0:(length-1)]
    , kappa ~ c_prop * delta / sum(delta)
    , Y ~ convolution(X, kappa)
  )
  
  self$user_formulas = function() {
    xxx
  }
  return_object(self, "GammaConvolution")
}


#' @noRd
to_change_component = function(x) UseMethod("to_change_component")

#' @export
to_change_component.ChangeComponent = function(x) x

#' @export
to_change_component.formula = function(x) Formula(x)

