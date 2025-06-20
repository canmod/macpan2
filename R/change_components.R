
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

print_flow_vec = function(vec) {
  components = sprintf("%s: %s", names(vec), unname(vec))
  paste0(components, collapse = "\n") |> cat()
}

#' @export
print.PerCapitaFlow = function(x, ...) {
  c(
      From = x$from
    , To = x$to
    , `Per-capita rate expression` = rhs_char(x$rate)
    , `Absolute rate symbol` = lhs_char(x$rate)
  ) |> print_flow_vec()
}

#' @export
print.AbsoluteFlow = function(x, ...) {
  c(
      From = x$from
    , To = x$to
    , `Absolute rate expression` = rhs_char(x$rate)
    , `Absolute rate symbol` = lhs_char(x$rate)
  ) |> print_flow_vec()
}


#' @export
print.AbsoluteInFlow = function(x, ...) {
  c(
      To = x$to
    , `Absolute rate expression` = rhs_char(x$rate)
    , `Absolute rate symbol` = lhs_char(x$rate)
  ) |> print_flow_vec()
}

#' @export
print.AbsoluteOutFlow = function(x, ...) {
  c(
      From = x$from
    , `Absolute rate expression` = rhs_char(x$rate)
    , `Absolute rate symbol` = lhs_char(x$rate)
  ) |> print_flow_vec()
}


#' @export
print.Formula = function(x, ...) print(x$formula, showEnv = FALSE)


FormulaList = function(formulas) {
  one_sided = vapply(formulas, is_one_sided, logical(1L))
  if (all(one_sided)) stop("Raw R formulas in an expression list must be two-sided")
  self = ChangeComponent()
  self$formulas = formulas
  self$user_formulas = function() as.list(self$formulas)
}

mp_sum = function(summand, sum = sprintf("%s_sum", summand)) {
  formula = two_sided(sum, sprintf("sum(%s)", summand))
  Formula(formula)
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

#' @importFrom stats pgamma qgamma
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
  
  self$user_formulas = function() {}
  return_object(self, "GammaConvolution")
}
