library(macpan2)
sir = Compartmental(file.path("inst", "starter_models", "sir"))
N = 100
time_steps = 100

derivation_to_math = function(derivation) {
  has_arguments = !is.null(derivation$arguments)
  has_dots = !is.null(derivation$argument_dots)
  if (has_arguments & has_dots) {
    return(MathExpressionFromStrings(derivation$expression,
      derivation$arguments, include_dots = TRUE))
  }
  else if (has_arguments) {
    return(MathExpressionFromStrings(derivation$expression,
      derivation$arguments, include_dots = FALSE))
  }
  else if (has_dots) {
    return(MathExpressionFromStrings(derivation$expression,
      include_dots = TRUE))
  }
  else stop("Derivations file appears invalid, no arguments or argument dots.")
}
order_by_phase = function(...) {
  phases = as.character(unlist(list(...), use.names = FALSE))
  function(...) {
    derivation_list = unlist(list(...), recursive = FALSE, use.names = FALSE)
    is_phase = function(phase) function(derivation) derivation$simulation_phase == phase
    filter_phase = function(phase) Filter(is_phase(phase), derivation_list)
    Reduce(c, lapply(phases, filter_phase))
  }
}
all_derivations = function(model) c(model$derivations(), StandardExpr(model)$as_derivations())
step_math_list = (sir
  |> all_derivations()
  |> order_by_phase("during_pre_update", "during_update")()
  |> lapply(derivation_to_math)
)

s = sir$simulators$tmb(time_steps = 10
  , state = c(S = 99, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.1)
  , N = 0, beta = 0.2
)
s$tmb_model$expr_list$during
