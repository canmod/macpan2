## This file is the user interface and associated utilities for 
## generating formula lists without requiring the user to generate
## them explicitly. The underlying class structure is in different
## files as follows.
## two class types:
##   1. ChangeModel (set of flows) (e.g. SimpleChangeModel) (change_models.R)
##   2. UpdateMethod (Euler, RK4, EulerMultinomial, TimeDerivative) (e.g. RK4UpdateMethod) (update_methods.R)
##
## one data structure type:
##   1. ChangeComponent (Flow, Birth, ...) (e.g. PerCapitaFlow) (change_components.R)



#' Per-Capita Flow (Experimental)
#' 
#' @param from String giving the name of the compartment from which the flow
#' originates.
#' @param to String giving the name of the compartment to which the flow is
#' going.
#' @param rate String giving the expression for the per-capita flow rate.
#' Alternatively, for back compatibility, a two-sided formula with the
#' left-hand-side giving the name of the absolute flow rate per unit time-step 
#' and the right-hand-side giving an expression for the per-capita rate of 
#' flow from `from` to `to`.
#' @param abs_rate String giving the name for the absolute flow rate,
#' which will be computed as `from * rate`. If a formula is passed to
#' `rate` (not recommended), then this `abs_rate` argument will be ignored.
#' 
#' @export
mp_per_capita_flow = function(from, to, rate, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate)
  PerCapitaFlow(from, to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment, and
#' do not flow out of the `from` compartment.
#' @export
mp_per_capita_inflow = function(from, to, rate, abs_rate = NULL) {
  call_string = deparse(match.call())
  rate = handle_rate_args(rate, abs_rate)
  PerCapitaInflow(from, to, rate, call_string)
}


##' State Updates (experimental)
##' 
##' Use these functions to update a model spec so that the state variables
##' are updated according to different numerical methods.
##' 
##' @param model Object with quantities that have been explicitly 
##' marked as state variables.
##' @export
mp_euler = function(model) UseMethod("mp_euler")

##' @describeIn mp_euler ODE solver using Runge-Kutta 4.
##' @export
mp_rk4 = function(model) UseMethod("mp_rk4")

##' @describeIn mp_euler Update state with process error given by the 
##' Euler-multinomial distribution. 
##' @export
mp_euler_multinomial = function(model) UseMethod("mp_euler_multinomial")

##' @describeIn mp_euler Update state with hazard steps, which is equivalent
##' to taking the step given by the expected value of the Euler-multinomial
##' distribution.
##' @export
mp_hazard = function(model) UseMethod("mp_hazard")

##' @export
mp_euler.TMBModelSpec = function(model) model$change_update_method("euler")

##' @export
mp_rk4.TMBModelSpec = function(model) model$change_update_method("rk4")

##' @export
mp_euler_multinomial.TMBModelSpec = function(model) model$change_update_method("euler_multinomial")

##' @export
mp_hazard.TMBModelSpec = function(model) model$change_update_method("hazard")


#' Expand Model
#' 
#' Expand a structured model so that it is represented in an unstructured
#' format requiring a more verbose description. Currently,
#' this is only applicable for \code{\link{mp_tmb_model_spec}} objects
#' that have explicit flows  
#' (e.g. \code{\link{mp_per_capita_flow}}). For such models, `mp_expand`
#' produces a model with expression lists composed entirely of plain R
#' formulas.
#' 
#' @param model A model object.
#'
#' @examples
#' sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
#' print(sir)
#' print(mp_expand(sir))
#' 
#' @export
mp_expand = function(model) UseMethod("mp_expand")

#' @export
mp_expand.TMBModelSpec = function(model) model$expand()


## Utilities

to_absolute_flows = function(per_capita_flows) {
  char_list = list()
  for (size_var in names(per_capita_flows)) {
    char_list = append(char_list, sprintf("%s ~ %s * %s"
      , vapply(per_capita_flows[[size_var]], lhs_char, character(1L))
      , size_var
      , vapply(per_capita_flows[[size_var]], rhs_char, character(1L))
    ))
  }
  lapply(char_list, as.formula)
}

get_state_update_type = function(state_update_type, change_model) {
  if (inherits(change_model, "AllFormulaChangeModel")) return("no")
  state_update_type
}
get_state_update_method = function(state_update, change_model) {
  if (inherits(change_model, "AllFormulaChangeModel")) {
    return(NoUpdateMethod(change_model))
  }
  cls_nm = sprintf("%sUpdateMethod", var_case_to_cls_case(state_update))
  if (state_update == "rk4") cls_nm = "RK4UpdateMethod"
  get(cls_nm)(change_model)
}
get_change_model = function(before, during, after) {
  valid_before = all(vapply(before, is_two_sided, logical(1L)))
  if (!valid_before) stop("The before argument must be all two-sided formulas.")
  valid_after = all(vapply(after, is_two_sided, logical(1L)))
  if (!valid_after) stop("The after argument must be all two-sided formulas.")
  any_change_components = any(vapply(during, inherits, logical(1L), "ChangeComponent"))
  if (any_change_components) return(SimpleChangeModel(before, during, after))
  AllFormulaChangeModel(before, during, after)
}
