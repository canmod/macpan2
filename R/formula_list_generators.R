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
#' `rate` (not recommended), then this `abs_rate_name` argument will be ignored.
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

##' Update Methods
##' 
##' Convert a `ChangeModel` into an object that can return `before`, `during`,
##' and `after` expression lists.
##' 
##' @param change_model An object of class `ChangeModel`.
##' @name update_methods
##' @noRd

NoUpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model 
  self$before = function() self$change_model$before_loop()
  self$during = function() self$change_model$before_flows()
  self$after = function() self$change_model$after_loop()
  return_object(self, "NoUpdateMethod")
}

EulerUpdateMethod = function(change_model, existing_global_names = character()) {
  self = UpdateMethod()
  self$change_model = change_model
  
  ## nb: euler method requires no additional names from the spec
  #self$existing_global_names = existing_global_names
  
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = to_absolute_flows(self$change_model$update_flows())
    before_update = self$change_model$before_state()
    update = self$change_model$update_state()
    
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    after_components = self$change_model$after_state()
    c(before_components, components, before_update, new_update, after_components)
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}


RK4UpdateMethod = function(change_model) {
  self = UpdateMethod()
  self$change_model = change_model
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    before_components = self$change_model$before_flows()
    components = to_absolute_flows(self$change_model$update_flows())
    before_state = self$change_model$before_state()
    before = c(before_components, components, before_state)
    update = self$change_model$update_state()
    
    new_update = list()
    new_before = list()
    
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    
    existing_names = self$change_model$all_variable_names()
    local_step_names = list(
        k1 = sprintf("k1_%s", states)
      , k2 = sprintf("k2_%s", states)
      , k3 = sprintf("k3_%s", states)
      , k4 = sprintf("k4_%s", states)
    )
    step_names = map_names(existing_names, local_step_names)
    
    ## rk4 step 1
    k1_new_before = before
    k1_new_update = sprintf("%s ~ %s", step_names$k1, rates) |> lapply(as.formula)
    
    ## rk4 step 2
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, step_names$k1) |> lapply(as.formula)
    k2_new_before = update_formulas(before, state_replacements)
    k2_new_update = sprintf("k2_%s ~ %s", states, rates) |> lapply(as.formula)
    
    ## rk4 step 3
    state_replacements = sprintf("%s ~ (%s + (%s / 2))", states, states, step_names$k2) |> lapply(as.formula)
    k3_new_before = update_formulas(before, state_replacements)
    k3_new_update = sprintf("k3_%s ~ %s", states, rates) |> lapply(as.formula)
    
    ## rk4 step 4
    state_replacements = sprintf("%s ~ (%s + %s)", states, states, step_names$k3) |> lapply(as.formula)
    k4_new_before = update_formulas(before, state_replacements)
    k4_new_update = sprintf("%s ~ %s", step_names$k4, rates) |> lapply(as.formula)
    
    ## final update step
    final_update = sprintf("%s ~ %s + (%s + 2 * %s + 2 * %s + %s)/6"
      , states, states, step_names$k1, step_names$k2, step_names$k3, step_names$k4
    ) |> lapply(as.formula) |> setNames(states)
    after_components = self$change_model$after_state()
    c(
        k1_new_before, k1_new_update
      , k2_new_before, k2_new_update
      , k3_new_before, k3_new_update
      , k4_new_before, k4_new_update
      , final_update, after_components
    )
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerUpdateMethod")
}


EulerMultinomialUpdateMethod = function(change_model) {
  self = Base()
  self$change_model = change_model
  
  self$vec = function(expr_list, char_fun) {
    vec = vapply(expr_list, char_fun, character(1L))
    simple_expr = all(grepl("^[a-zA-Z0-9]+$", vec))
    scalar_expr = length(vec) == 1L
    
    if (simple_expr & scalar_expr) return(vec)
    if (!simple_expr | !scalar_expr) {
      vec = sprintf("(%s)", paste0(vec, collapse = ", "))
    }
    if (!scalar_expr) {
      vec = sprintf("c%s", vec)
    }
    return(vec)
  }
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    flow_list = self$change_model$update_flows()
    before_components = self$change_model$before_flows()
    before_state = self$change_model$before_state()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ reulermultinom(%s, %s)"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
      )
    }
    new_flow = lapply(components, as.formula)
    
    update = self$change_model$update_state()
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    
    c(before_components, new_flow, before_state, new_update)
  }
  self$after = function() self$change_model$after_loop()
  return_object(self, "EulerMultinomialUpdateMethod")
}

HazardUpdateMethod = function(change_model) {
  self = EulerMultinomialUpdateMethod(change_model)
  self$during = function() {

    flow_list = self$change_model$update_flows()
    components = list()
    for (size_var in names(flow_list)) {
      components[[size_var]] = sprintf("%s ~ %s * (1 - exp(-sum(%s))) * %s / (sum(%s))"
        , self$vec(flow_list[[size_var]], lhs_char)
        , size_var
        , self$vec(flow_list[[size_var]], rhs_char)
        , self$vec(flow_list[[size_var]], rhs_char)
        , self$vec(flow_list[[size_var]], rhs_char)
      )
    }
    new_flow = lapply(components, as.formula)
    
    update = self$change_model$update_state()
    states = vapply(update, lhs_char, character(1L))
    rates = vapply(update, rhs_char, character(1L))
    update_char = sprintf("%s ~ %s %s", states, states, rates)
    new_update = lapply(update_char, as.formula)
    
    before_components = self$change_model$before_flows()
    before_state = self$change_model$before_state()
    after_components = self$change_model$after_state()
    
    c(before_components, new_flow, before_state, new_update, after_components)
  }
  return_object(self, "HazardUpdateMethodUpdateMethod")
}


# Change Components

#' Per-Capita Flow (Experimental)
#' 
#' @param from String giving the name of the compartment from which the flow
#' originates.
#' @param to String giving the name of the compartment to which the flow is
#' going.
#' @param rate A two-sided formula with the left-hand-side giving the name of
#' the absolute flow rate per unit time-step and the right-hand-side giving 
#' an expression for the per-capita rate of flow from `from` to `to`.
#' 
#' @export
mp_per_capita_flow = function(from, to, rate) {
  call_string = deparse(match.call())
  PerCapitaFlow(from, to, rate, call_string)
}

#' @describeIn mp_per_capita_flow Only flow into the `to` compartment, and
#' do not flow out of the `from` compartment.
#' @export
mp_per_capita_inflow = function(from, to, rate) {
  call_string = deparse(match.call())
  PerCapitaInflow(from, to, rate, call_string)
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


#' @noRd
to_change_component = function(x) UseMethod("to_change_component")

#' @export
to_change_component.ChangeComponent = function(x) x

#' @export
to_change_component.formula = function(x) Formula(x)


#' Reduce Model
#' 
#' Reduce a model by removing any model structure 
#' (e.g. \code{\link{mp_per_capita_flow}}), so that expression lists
#' are plain R formulas.
#' 
#' @param model A model object.
#'
#' @export
mp_reduce = function(model) UseMethod("mp_reduce")

#' @export
mp_reduce.TMBModelSpec = function(model) model$expand()
