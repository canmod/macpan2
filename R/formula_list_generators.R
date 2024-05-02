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


## two class types:
##   1. ChangeModel (e.g. set of flows)
##   2. UpdateMethod (e.g. Euler, RK4, EulerMultinomial, TimeDerivative)
##
## one data structure type:
##   1. ChangeComponent (e.g. Flow, Birth, ...)

## Abstract Classes
## 
## ChangeModel -- functions for specifying what kinds of expressions should
## go in what phases of the simulation. more finely grained than just
## before-during-after, and makes absolute flow and state updates first
## class citizens. importantly this is not what creates the final expression
## lists -- something that is done by UpdateMethod
ChangeModel = function() {
  self = Base()
  
  self$before_loop = function() list()
  self$before_flows = function() list()
  self$update_flows = function() list()
  self$before_state = function() list()
  self$update_state = function() list()
  self$after_state = function() list()
  self$after_loop = function() list()
  
  ## not quite abstract, but useful for now at least.
  ## these frame functions bind the rows of the 
  ## change frames associated with each change component. 
  self$flow_frame = function() {
    ## TODO: check for change_list
    (self$change_list
      |> oor::method_apply("flow_frame")
      |> bind_rows()
    )
  }
  self$change_frame = function() {
    (self$change_list
      |> oor::method_apply("change_frame")
      |> bind_rows()
    )
  }
  self$change_classes = function() {
    (self$change_list
      |> lapply(class)
      |> vapply(getElement, character(1L), 1L)
    )
  }
  self$user_formulas = function() {
    method_apply(self$change_list, "user_formulas")
  }
  self$all_variable_names = function() {
    quasi_during_exprs = c(
      unlist(self$flow_frame(), use.names = FALSE, recursive = FALSE), 
      unlist(self$change_frame(), use.names = FALSE, recursive = FALSE)
    ) |> unique() |> sprintf(fmt = " ~ %s") |> lapply(as.formula)
    user_formulas = unlist(self$user_formulas(), recursive = FALSE, use.names = FALSE)
    exprs = c(
        self$before_loop()
      , quasi_during_exprs
      , user_formulas
      , self$after_loop()
    )
    (exprs
      |> lapply(formula_components, side = "both")
      |> lapply(getElement, "variables")
      |> unlist(use.names = FALSE, recursive = FALSE)
      |> unique()
    )
  }
  return_object(self, "ChangeModel")
}

## UpdateMethod -- functions for rendering the final before-during-after
## expression lists to be used in simulations
UpdateMethod = function(exising_global_names = character()) {
  self = Base()
  self$existing_global_names = exising_global_names
  self$before = function() list()
  self$during = function() list()
  self$after = function() list()
  return_object(self, "UpdateMethod")
}

## ChangeComponent -- functions for producing the change_frame and flow_frame,
## which describe model dynamics
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
  
  self$user_formulas = function() list()
  
  self$string = function() "Abstract change component"
  
  return_object(self, "ChangeComponent")
}


## Change Models

##' Simple Change Model
##' 
##' @param before List of two-sided formulas to be run in the before phase.
##' @param during List of two-sided formulas and/or `ChangeComponent` objects
##' to be used to create a `during` expression list after choosing an
##' `UpdateMethod`.
##' @param after List of two-sided formulas to be run in the after phase.
##' @examples
##' 
##' si = SimpleChangeModel(
##'   during = list(mp_per_capita_flow("S", "I", infection ~ beta * I / N))
##' ) |> EulerUpdateMethod()
##' si$during()
##' 
##' @noRd
SimpleChangeModel = function(before = list(), during = list(), after = list()) {
  self = ChangeModel()
  
  self$before = before
  self$during = during
  self$after = after
  
  self$change_list = lapply(self$during, to_change_component)
  
  ## find what formulas were provided by the user as raw formulas
  ## and make sure that they know that they should place them either
  ## at the beginning or the end of the during list
  head_indices = tail_indices = integer()
  is_formula = self$change_classes() == "Formula"
  if (all(is_formula)) {
    head_indices = seq_along(is_formula)
  }
  else if (length(is_formula) > 0L) {
    if (isTRUE(head(is_formula, 1L))) {
      first_non_formula = which.min(is_formula)
      head_indices = seq_len(first_non_formula - 1L)
    }
    if (isTRUE(tail(is_formula, 1L))) {
      tail_indices = length(is_formula) - ((which.min(rev(is_formula)) - 1L):1) + 1L
    }
    if (sum(is_formula) > (length(head_indices) + length(tail_indices))) {
      warning("Raw formula-valued expressions were inserted between flow-based expressions. These raw formulas will be ignored. Please collect raw formulas at the start or end of the during list")
    }
  }
  self$.before_flows = self$user_formulas()[head_indices]
  self$.after_state = self$user_formulas()[tail_indices]
  
  self$no_change_components = function() all(self$change_classes() == "Formula")
  
  self$before_flows = function() {
    unlist(self$.before_flows, recursive = FALSE, use.names = FALSE)
  }
  self$after_state = function() {
    unlist(self$.after_state, recursive = FALSE, use.names = FALSE)
  }
  
  self$update_flows = function() {
    frame = self$flow_frame()
    size_vars = unique(frame$size)
    flow_list = list()
    formulas = sprintf("%s ~ %s", frame$change, frame$rate) |> lapply(as.formula)
    for (var in size_vars) {
      flow_list[[var]] = formulas[frame$size == var]
    }
    flow_list
  }
  self$update_state = function() {
    frame = self$change_frame()
    exprs = tapply(frame$change, frame$state, paste0, collapse = " ", simplify = TRUE)[unique(frame$state)]
    sprintf("%s ~ %s", names(exprs), unname(exprs)) |> lapply(as.formula)
  }
  self$before_loop = function() self$before
  self$after_loop = function() self$after
  
  return_object(self, "SimpleChangeModel")
}

AllFormulaChangeModel = function(before = list(), during = list(), after = list()) {
  self = ChangeModel()
  self$before = before
  self$during = during
  self$after = after
  
  self$before_loop = function() self$before
  self$before_flows = function() self$during
  self$after_loop = function() self$after
  
  return_object(self, "AllFormulaChangeModel")
}


MockChangeModel = function() {
  self = ChangeModel()
  
  self$before_loop = function() list(S ~ N - I - V)
  self$before_flows = function() list(N ~ S + I + V)
  
  ## TODO: how to handle this method when we move to vector-valued 
  ## flow components and states
  ## TODO: make sure that in the scalar case at least the names are 
  ## unique in the output list
  self$update_flows = function() {
    list(
      # {capita} = list(
      #     {update_component_1} ~ {expression_giving_per_capita_flow_1}
      #   , ...
      #   , {update_component_n} ~ {expression_giving_per_capita_flow_n}
      # )
        N = list(birth ~ mu)
      , S = list(
            infection ~ beta * I / N
          , vax_uptake ~ rho * vax_supply
          , death_S ~ mu
        )
      , I = list(death_I ~ mu)
      , V = list(death_V ~ mu)
    )
  }
  
  self$update_state = function() list(
      S ~ birth - infection - vax_uptake - death_S
    , I ~ infection - recovery - death_I
    , V ~ vax_uptake - death_V
    , vax_supply ~ vax_production - vax_uptake - vax_expiry
  )
  
  self$default = function() list()
  self$integers = function() list()
  
  return_object(self, "ChangeModel")
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

##' @describeIn mp_euler Update state with process error given  by the 
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
    if (length(vec) == 1L) return(vec)
    sprintf("c(%s)", paste0(vec, collapse = ", "))
  }
  
  self$before = function() self$change_model$before_loop()
  self$during = function() {
    flow_list = self$change_model$update_flows()
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
    
    c(new_flow, new_update)
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
    
    c(new_flow, new_update)
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
